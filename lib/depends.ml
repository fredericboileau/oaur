open Lwt.Syntax
open Errors
open Utils

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Cohttp.Header.init_with "User-Agent" "oaur"
let aur_callback_max = 30 [@@deriving show]

type dep_type = Depends | MakeDepends | CheckDepends | OptDepends | Self

let all_dep_types = [ Depends; MakeDepends; CheckDepends; OptDepends ]

type aur_pkg = {
  name : string; [@key "Name"]
  package_base : string; [@key "PackageBase"]
  version : string; [@key "Version"]
  depends : string list; [@key "Depends"] [@default []]
  make_depends : string list; [@key "MakeDepends"] [@default []]
  check_depends : string list; [@key "CheckDepends"] [@default []]
  opt_depends : string list; [@key "OptDepends"] [@default []]
  provides : string list; [@key "Provided"] [@default []]
  required_by : (string * dep_type) list; [@default []]
}
[@@deriving yojson { strict = false }, show]

let expand_deps_from_type pkg deptype =
  match deptype with
  | Depends -> pkg.depends
  | MakeDepends -> pkg.make_depends
  | CheckDepends -> pkg.check_depends
  | OptDepends -> pkg.opt_depends
  | Self -> []

let strip_version dep =
  let re = Str.regexp {|[<>=]|} in
  match Str.search_forward re dep 0 with
  | i -> String.sub dep 0 i
  | exception Not_found -> dep

let query_aur_info pkgnames =
  let aururl = Uri.of_string aur_location in
  let path = "/rpc/v" ^ string_of_int aur_rpc_ver ^ "/info" in
  let url = Uri.with_path aururl path in
  let headers =
    Cohttp.Header.add ua_header "Content-Type"
      "application/x-www-form-urlencoded"
  in
  let body_str =
    String.concat "&" (List.map (fun p -> "arg[]=" ^ Uri.pct_encode p) pkgnames)
  in
  let body = Cohttp_lwt.Body.of_string body_str in
  let open Cohttp_lwt_unix in
  let* _, body = Client.post ~headers ~body url in
  Cohttp_lwt.Body.to_string body

let recurse pkgnames types =
  (*information for all aur pkgs queried with rpc*)
  let results : (string, aur_pkg) Hashtbl.t = Hashtbl.create 16 in
  (*pkg -> depspec (includes version) and dep_type*)
  let pkgdeps : (string, (string * dep_type) list) Hashtbl.t =
    Hashtbl.create 16
  in
  (* key is virtpkg provided and value is (pkg provider, pkg provider version)*)
  let pkgmap : (string, string * string) Hashtbl.t = Hashtbl.create 16 in
  (*so we dont query deps for the same package twice*)
  let tally : (string, bool) Hashtbl.t = Hashtbl.create 16 in

  let fetchinfo pkgnames =
    let* body = query_aur_info pkgnames in
    let open Yojson.Safe.Util in
    let result = Yojson.Safe.from_string body |> member "results" |> to_list in
    Lwt.return result
  in

  let rec resolve depth batch =
    if depth >= aur_callback_max then begin
      Printf.eprintf "Total requests: %d (out of range)" depth;
      exit 1
    end;
    if batch = [] then Lwt.return ()
    else
      let* json_list = fetchinfo batch in
      let level =
        List.filter_map
          (fun j ->
            match aur_pkg_of_yojson j with Ok p -> Some p | Error _ -> None)
          json_list
      in

      let all_deps pkg =
        List.concat_map
          (fun deptype ->
            List.map
              (fun dep -> (dep, deptype))
              (expand_deps_from_type pkg deptype))
          types
      in

      let next =
        List.concat_map
          (fun pkg ->
            List.iter
              (fun prov_spec ->
                match String.split_on_char '=' prov_spec with
                | [ prov; prov_version ] ->
                    Hashtbl.add pkgmap prov (pkg.name, prov_version)
                | [ prov ] -> Hashtbl.add pkgmap prov (pkg.name, "")
                | _ -> ())
              pkg.provides;

            Hashtbl.replace results pkg.name pkg;
            Hashtbl.replace tally pkg.name true;

            List.filter_map
              (fun (dep, deptype) ->
                Hashtbl.replace pkgdeps pkg.name
                  ((dep, deptype)
                  :: Option.value ~default:[]
                       (Hashtbl.find_opt pkgdeps pkg.name));
                let bare_dep = strip_version dep in
                if Hashtbl.mem tally bare_dep then None
                else begin
                  Hashtbl.replace tally bare_dep true;
                  Some bare_dep
                end)
              (all_deps pkg))
          level
      in

      resolve (depth + 1) next
  in

  (* Populate depends map with command-line targets (#1136) *)
  List.iter (fun name -> Hashtbl.add pkgdeps name [ (name, Self) ]) pkgnames;
  (*trigger the run*)
  let* () = resolve 1 pkgnames in

  (*check results*)
  if Hashtbl.length results = 0 then begin
    Printf.eprintf "no packages found\n";
    exit 1
  end
  else
    List.iter
      (fun name ->
        if not (Hashtbl.mem results name) then
          Printf.eprintf "target not found %s\n" name)
      pkgnames;

  Lwt.return (results, pkgdeps, pkgmap)

let dag_insert dag prov name dep_type =
  let inner =
    match Hashtbl.find_opt dag prov with
    | Some t -> t
    | None ->
        let t = Hashtbl.create 4 in
        Hashtbl.replace dag prov t;
        t
  in
  Hashtbl.replace inner name dep_type

let graph ~provides ~verify results pkgdeps pkgmap =
  let dag : (string, (string, dep_type) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 16
  in
  let dag_foreign : (string, (string, dep_type) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 16
  in

  let parse_dep dep_spec =
    let re = Str.regexp {|<=\|>=\|<\|=\|>|} in
    match Str.full_split re dep_spec with
    | [ Text name; Delim op; Text req ] -> (name, Some op, Some req)
    | [ Text name ] -> (name, None, None)
    | _ -> failwith (Printf.sprintf "parse_dep: unepected format %s" dep_spec)
  in

  (* use vercmp from archlinux distribution with vercmp_run *)
  let vercmp ver1 ver2 op =
    let vercmp_run ver1 ver2 =
      int_of_string
        (String.trim (run_read_all ("vercmp", [ "vercmp"; ver1; ver2 ])))
    in
    match (ver2, op) with
    | None, _ | _, None -> true
    | Some v2, Some "=" -> ver1 = v2
    | Some v2, Some op -> (
        let cmp = vercmp_run ver1 v2 in
        match op with
        | "<" -> cmp < 0
        | ">" -> cmp > 0
        | "<=" -> cmp <= 0
        | ">=" -> cmp >= 0
        | _ -> failwith ("invalid vercmp operation: " ^ op))
  in

  (* build dag from pkgdeps hashtbl using pkgmap for virtual packages *)
  (* build dag-foreign if doesn't find package in results *)
  Hashtbl.iter
    (fun name deps ->
      List.iter
        (fun (dep_spec, dep_type) ->
          let dep_name, dep_op, dep_req = parse_dep dep_spec in
          match Hashtbl.find_opt results dep_name with
          | Some pkg ->
              let dep_ver = List.hd (Str.split (Str.regexp "-") pkg.version) in
              let prov_name, prov_ver =
                if provides && Hashtbl.mem pkgmap dep_name then
                  Hashtbl.find pkgmap dep_name
                else (dep_name, dep_ver)
              in
              if (not verify) || vercmp prov_ver dep_req dep_op then
                dag_insert dag prov_name name dep_type
              else begin
                Printf.eprintf "invalid node: %s=%s (required: %s%s by %s)"
                  prov_name prov_ver
                  (Option.value ~default:"None" dep_op)
                  (Option.value ~default:"None" dep_req)
                  name;
                exit 1
              end
          | None -> dag_insert dag_foreign dep_name name dep_type)
        deps)
    pkgdeps;

  (dag, dag_foreign)

(*remove installed packages from dag*)
(*first remove installed and then recurse on orphans*)

(*TODO FP instead of mutating dag in place*)
let prune dag installed =
  let module StringSet = Set.Make (String) in
  let set_of_provs dag =
    Hashtbl.fold (fun prov _ set -> StringSet.add prov set) dag StringSet.empty
  in
  let installed_set =
    List.fold_left
      (fun set inst -> StringSet.add inst set)
      StringSet.empty installed
  in
  let rec go toremove =
    if StringSet.is_empty toremove then []
    else begin
      let prev_provs = set_of_provs dag in
      (* remove dependents that are in toremove from each inner table *)
      Hashtbl.iter
        (fun _prov inner ->
          Hashtbl.filter_map_inplace
            (fun dep dep_type ->
              if StringSet.mem dep toremove then None else Some dep_type)
            inner)
        dag;
      (* remove providers that are in toremove or have no dependents left *)
      Hashtbl.filter_map_inplace
        (fun prov inner ->
          if StringSet.mem prov toremove || Hashtbl.length inner = 0 then None
          else Some inner)
        dag;
      let curr_provs = set_of_provs dag in
      let removals = StringSet.diff prev_provs curr_provs in
      StringSet.elements removals @ go removals
    end
  in
  go installed_set

let solve ~installed ~verify ~provides types targets =
  let* results, pkgdeps, pkgmap = recurse targets types in
  let dag, dag_foreign = graph ~provides ~verify results pkgdeps pkgmap in
  let to_prune =
    installed
    @ if provides then Hashtbl.fold (fun k _ acc -> k :: acc) pkgmap [] else []
  in
  let removed = prune dag to_prune in
  List.iter (fun name -> Hashtbl.remove results name) removed;

  Lwt.return (results, dag, dag_foreign)

let main ?(include_depends = true) ?(include_makedepends = true)
    ?(include_checkdepends = true) ?(include_optdepends = false)
    ?(installed = []) ?(verify = true) ?(provides = true) targets =
  let types =
    List.filter_map
      (fun (flag, dep_type) -> if flag then Some dep_type else None)
      [
        (include_depends, Depends);
        (include_makedepends, MakeDepends);
        (include_checkdepends, CheckDepends);
        (include_optdepends, OptDepends);
      ]
  in
  let* results, dag, dag_foreign =
    solve ~installed ~verify ~provides types targets
  in

  Hashtbl.iter
    (fun prov inner ->
      match Hashtbl.find_opt results prov with
      | Some pkg ->
          let req_by = Hashtbl.fold (fun dep dt acc -> (dep, dt) :: acc) inner [] in
          Hashtbl.replace results prov { pkg with required_by = req_by }
      | None -> ())
    dag;

  Lwt.return ()

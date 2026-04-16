open Lwt.Syntax
open Utils

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Cohttp.Header.init_with "User-Agent" "oaur"
let aur_callback_max = 30 [@@deriving show]

type output_mode = Pairs | Table | Json | JsonLines

type dep_type = Depends | MakeDepends | CheckDepends | OptDepends | Self
[@@deriving yojson, show]

let all_dep_types = [ Depends; MakeDepends; CheckDepends; OptDepends ]

let dep_type_to_string = function
  | Depends -> "Depends"
  | MakeDepends -> "MakeDepends"
  | CheckDepends -> "CheckDepends"
  | OptDepends -> "OptDepends"
  | Self -> "Self"

(*override serializers / deserializers for deptype variant pair into flat dict with strings*)
let required_by_to_yojson lst =
  `Assoc
    (List.map (fun (name, dt) -> (name, `String (dep_type_to_string dt))) lst)

let required_by_of_yojson = function
  | `Assoc pairs ->
      Result.ok
        (List.filter_map
           (fun (name, v) ->
             match v with
             | `String s -> (
                 match dep_type_of_yojson (`List [ `String s ]) with
                 | Ok dt -> Some (name, dt)
                 | Error _ -> None)
             | _ -> None)
           pairs)
  | _ -> Error "required_by_of_yojson: expected object"

type aur_pkg = {
  name : string; [@key "Name"]
  package_base : string option; [@key "PackageBase"] [@default None]
  version : string option; [@key "Version"] [@default None]
  depends : string list; [@key "Depends"] [@default []]
  make_depends : string list; [@key "MakeDepends"] [@default []]
  check_depends : string list; [@key "CheckDepends"] [@default []]
  opt_depends : string list; [@key "OptDepends"] [@default []]
  provides : string list; [@key "Provides"] [@default []]
  description : string option; [@key "Description"] [@default None]
  url : string option; [@key "URL"] [@default None]
  url_path : string option; [@key "URLPath"] [@default None]
  num_votes : int option; [@key "NumVotes"] [@default None]
  popularity : float option; [@key "Popularity"] [@default None]
  out_of_date : int option; [@key "OutOfDate"] [@default None]
  maintainer : string option; [@key "Maintainer"] [@default None]
  submitter : string option; [@key "Submitter"] [@default None]
  first_submitted : int option; [@key "FirstSubmitted"] [@default None]
  last_modified : int option; [@key "LastModified"] [@default None]
  id : int option; [@key "ID"] [@default None]
  package_base_id : int option; [@key "PackageBaseID"] [@default None]
  keywords : string list; [@key "Keywords"] [@default []]
  license : string list; [@key "License"] [@default []]
  (*added after parsing, does not come from json*)
  required_by : (string * dep_type) list;
      [@default []]
      [@to_yojson required_by_to_yojson]
      [@of_yojson required_by_of_yojson]
}
[@@deriving yojson { strict = false }, show]

let empty_pkg name =
  {
    name;
    required_by = [];
    package_base = None;
    version = None;
    depends = [];
    make_depends = [];
    check_depends = [];
    opt_depends = [];
    provides = [];
    description = None;
    url = None;
    url_path = None;
    num_votes = None;
    popularity = None;
    out_of_date = None;
    maintainer = None;
    submitter = None;
    first_submitted = None;
    last_modified = None;
    id = None;
    package_base_id = None;
    keywords = [];
    license = [];
  }

let expand_deps_from_type pkg deptype =
  match deptype with
  | Depends -> pkg.depends
  | MakeDepends -> pkg.make_depends
  | CheckDepends -> pkg.check_depends
  | OptDepends -> pkg.opt_depends
  | Self -> []

let all_deps pkg types =
  List.concat_map
    (fun deptype ->
      List.map (fun dep -> (dep, deptype)) (expand_deps_from_type pkg deptype))
    types

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
      (*TODO error out on aur_pkg_of_json failure*)
      let level =
        List.filter_map
          (fun j ->
            match aur_pkg_of_yojson j with Ok p -> Some p | Error _ -> None)
          json_list
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
              (all_deps pkg types))
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
      int_of_string @@ run_capture "vercmp" [ ver1; ver2 ]
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
              let dep_ver =
                List.hd
                  (Str.split (Str.regexp "-")
                     (Option.value ~default:"" pkg.version))
              in
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

let add_dag_to_results results dag dag_foreign show_all =
  Hashtbl.iter
    (fun name inner ->
      let req_by = Hashtbl.fold (fun dep dt acc -> (dep, dt) :: acc) inner [] in
      (*find instead of find_opt, we are guaranteed there is an entry in results if in dag*)
      let pkg = Hashtbl.find results name in
      Hashtbl.replace results name { pkg with required_by = req_by })
    dag;

  if show_all then
    Hashtbl.iter
      (fun name inner ->
        let required_by =
          Hashtbl.fold (fun dep dt acc -> (dep, dt) :: acc) inner []
        in
        let pkg = { (empty_pkg name) with required_by } in
        Hashtbl.add results name pkg)
      dag_foreign

let make_pairs results ~key ~reverse =
  let seen : (string * string, bool) Hashtbl.t = Hashtbl.create 16 in
  let get_id pkg =
    match key with
    | `Name -> pkg.name
    | `PackageBase -> Option.value ~default:"-" pkg.package_base
  in
  let get_rdep dep_name =
    match key with
    | `Name -> dep_name
    | `PackageBase -> (
        match Hashtbl.find_opt results dep_name with
        | Some pkg -> Option.value ~default:"-" pkg.package_base
        | None -> "-")
  in
  Hashtbl.fold
    (fun _ pkg acc ->
      let target = get_id pkg in
      List.fold_left
        (fun acc (dep_name, _dep_type) ->
          let rdep = get_rdep dep_name in
          let pair = if reverse then (target, rdep) else (rdep, target) in
          if Hashtbl.mem seen pair then acc
          else begin
            Hashtbl.replace seen pair true;
            pair :: acc
          end)
        acc pkg.required_by)
    results []

type table_row = {
  name : string;
  dep : string;
  base : string;
  version : string;
  dep_type : dep_type;
}

let make_table_v10 results types =
  Hashtbl.fold
    (fun name pkg acc ->
      match pkg.package_base with
      | None -> acc
      | Some base ->
          let pkg_row =
            {
              name;
              dep = name;
              base;
              version = Option.value ~default:"-" pkg.version;
              dep_type = Self;
            }
          in
          let rows =
            List.fold_left
              (fun acc_rows (dep, dep_type) ->
                {
                  name;
                  dep;
                  base;
                  version = Option.value ~default:"-" pkg.version;
                  dep_type;
                }
                :: acc_rows)
              [] (all_deps pkg types)
          in
          (pkg_row :: rows) @ acc)
    results []

let make_table_reverse results =
  Hashtbl.fold
    (fun name pkg acc ->
      let base = Option.value ~default:"-" pkg.package_base in
      let version = Option.value ~default:"-" pkg.version in
      List.fold_left
        (fun acc (dep, dep_type) ->
          { name; dep; base; version; dep_type } :: acc)
        acc pkg.required_by)
    results []

let print_table rows =
  List.iter
    (fun row ->
      print_endline
        (String.concat "\t"
           [
             row.name;
             row.dep;
             row.base;
             row.version;
             show_dep_type row.dep_type;
           ]))
    rows

(*handle "-" to take package from stdin*)
let main ?(include_depends = true) ?(include_makedepends = true)
    ?(include_checkdepends = true) ?(include_optdepends = false)
    ?(installed = []) ?(verify = true) ?(provides = true) ?(show_all = false)
    ?(output_mode = Pairs) ?(opt_pkgname = false) ?(opt_reverse = false) targets
    =
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

  add_dag_to_results results dag dag_foreign show_all;

  Lwt.return
    (match output_mode with
    | Pairs ->
        let pairs =
          make_pairs results
            ~key:(if opt_pkgname || show_all then `Name else `PackageBase)
            ~reverse:opt_reverse
        in
        List.iter (fun (x, y) -> print_endline (x ^ "\t" ^ y)) pairs
    | Table ->
        print_table
          (if opt_reverse then make_table_v10 results types
           else make_table_reverse results)
    | Json ->
        let json =
          `Assoc
            (Hashtbl.fold
               (fun name pkg acc -> (name, aur_pkg_to_yojson pkg) :: acc)
               results [])
        in
        print_endline (Yojson.Safe.to_string json)
    | JsonLines ->
        Hashtbl.iter
          (fun _name pkg ->
            print_endline (Yojson.Safe.to_string (aur_pkg_to_yojson pkg)))
          results)

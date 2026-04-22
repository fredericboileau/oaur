open Utils

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Cohttp.Header.init_with "User-Agent" "oaur"

(* TODO: check how alad installs pacman deps *)
(* TODO: supress backtraces when printing error messages *)

let search term =
  let search_aur term =
    let aururl = Uri.of_string aur_location in
    let path = "/rpc" ^ "/v" ^ string_of_int aur_rpc_ver ^ "/search/" ^ term in
    let query = Uri.with_path aururl path in
    let open Lwt.Syntax in
    let open Cohttp_lwt_unix in
    let* _, body = Client.get ?headers:(Some ua_header) query in
    Cohttp_lwt.Body.to_string body
  in
  let display_search_results body =
    let osc8_link uri label =
      let st = "\o033\\" in
      let osc8 = "\o033]8" in
      Printf.sprintf "%s;;%s%s%s%s;;%s" osc8 uri st label osc8 st
    in
    let colored_pkg_name name =
      let fmt = Format.get_str_formatter () in
      Ocolor_format.prettify_formatter fmt;
      Format.fprintf fmt "@{<bold>@{<blue>aur@}/%s@}" name;
      Format.flush_str_formatter ()
    in
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    let results = from_string body |> member "results" |> to_list in
    List.iter
      (fun result ->
        let name = member "Name" result |> to_string in
        let ver = member "Version" result |> to_string in
        let numvotes = member "NumVotes" result |> to_int |> string_of_int in
        let popularity = member "Popularity" result |> to_number in
        let descr = member "Description" result |> to_string in
        let ood =
          match member "OutOfDate" result |> to_number_option with
          | None -> ""
          | Some epoch ->
              Core_unix.strftime (Core_unix.gmtime epoch)
                "(Out-of-date: %d %B %Y)"
        in
        let label = colored_pkg_name name in
        let pre = osc8_link (aur_location ^ "/packages/" ^ name) label in
        let rest_fmt : _ format =
          "%s @{<bold>@{<green>%s@}@} (+%s %.2f%%) @{<bold;red>%s@}\n    %s\n"
        in
        Ocolor_format.printf rest_fmt pre ver numvotes popularity ood descr)
      results;
    Lwt.return ()
  in
  let open Lwt.Syntax in
  let* body = search_aur term in
  display_search_results body

let fetch_exn syncmode discard pkgname =
  let pkglocation = aur_location ^ "/" ^ pkgname in
  let status = run "git" [ "ls-remote"; "--exit-code"; "-q" ] in
  ignore
    (match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code -> raise (Errors.SubExn ("git ls-remote --exit-code -q", code))
    | _ -> raise (Errors.SubExn ("git ls-remote --exit-code -q", 1)));

  let pathtocheck = Filename.concat pkgname ".git" in
  let pathclean =
    not (Sys.file_exists pathtocheck && Sys.is_directory pathtocheck)
  in

  (*TODO add results file? *)
  if pathclean then run_exn "git" [ "clone"; pkglocation ]
  else
    let git = [ "git"; "-C"; pkgname ] in
    let ( @ ) = List.append in
    let sync_should_merge upstream dest =
      let status =
        run_with_arg0 "git"
          (git @ [ "merge-base"; "--is-ancestor"; upstream; dest ])
      in
      match status with
      | Unix.WEXITED 0 -> false
      | Unix.WEXITED 1 -> true
      | _ -> raise (Errors.SubExn ("git merge-base --is-ancestor", 1))
    in
    (* sync code goes here*)

    (*TODO write new wrapper of flock*)

    (* let fd = Unix.openfile pathtocheck [Unix.O_RDONLY] 0o640 in *)
    (* Flock.flock fd LOCK_EX; *)
    let _ = run_with_arg0 "git" (git @ [ "fetch"; "origin" ]) in

    (* let orig_head = String.trim( *)
    (*     run_read_all("git", git @ ["rev-parse"; "--verify"; "HEAD"])) in *)
    let should_merge = sync_should_merge "origin/HEAD" "HEAD" in
    let upstream = "origin/HEAD" in
    if should_merge then
      let syncmode = Option.value ~default:"merge" syncmode in
      let dest =
        match syncmode with
        | "merge" | "rebase" -> "HEAD"
        | "reset" | "fetch" -> upstream
        | badsyncmode ->
            failwith (Printf.sprintf "Bad syncmode: %s" badsyncmode)
      in
      match syncmode with
      | "merge" ->
          if discard then
            let _ = run_with_arg0 "git" (git @ [ "checkout"; "./" ]) in
            ignore (run_with_arg0 "git" (git @ [ "merge"; upstream ]))
      | "rebase" ->
          if discard then
            let _ = run_with_arg0 "git" (git @ [ "checkout"; "./" ]) in
            ignore (run_with_arg0 "git" (git @ [ "rebase"; upstream ]))
      | "reset" ->
          ignore (run_with_arg0 "git" (git @ [ "reset"; "--hard"; dest ]))
      | "fetch" -> ()
      | badsyncmode -> failwith (Printf.sprintf "Bad syncmode: %s" badsyncmode)

(* Flock.flock fd LOCK_UN; *)

(*spec: if --recurse passed use depends with a tsort algo *)
(*  to generate list, fetch should then handle lists and the *)
(*  argument passed by cli should be wrapped in said list *)
(*TODO cli should pass list*)

let fetch pkgnames syncmode discard =
  List.iter (fetch_exn syncmode discard) pkgnames

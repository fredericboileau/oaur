open Utils
open Errors

let strip_prefix prefix s =
  if String.starts_with ~prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

(**returns db_name, db_root and db_path, if db_name provided just resolve corresponding db_root
   otherwise find file repos. If only one file repos return corresponding db_name and db_root
   otherwise raise error either because none or because more than one is ambiguous. If both
   db_name and db_root provided just resolve db_path**)
let resolve_db ?pacman_conf ?db_ext ?db_name ?db_root () =
  let conf_args = match pacman_conf with Some p -> [ "--config"; p ] | None -> [] in
  (*autodetect if no dbname provided*)
  let find_file_repos () =
    let repos =
      String.split_on_char '\n' (run_capture "pacman-conf" @@ conf_args @ [ "--repo-list" ])
    in
    List.filter_map
      (fun repo_name ->
        if repo_name = "" then None
        else
          run_capture "pacman-conf" @@ conf_args @ [ "-r"; repo_name; "Server" ]
          |> String.split_on_char '\n' |> List.hd |> strip_prefix "file://"
          |> Option.map (fun root -> (repo_name, root)))
      repos
  in
  let db_name, db_root =
    match (db_name, db_root) with
    | Some name, Some root -> (name, root)
    | Some name, None -> (
        let server =
          run_capture "pacman-conf" @@ conf_args @ [ "-r"; name; "Server" ]
          |> String.split_on_char '\n' |> List.hd
        in
        match strip_prefix "file://" server with
        | Some r -> (name, r)
        | None -> raise (UsageError (Printf.sprintf "%s: not a local repository" server))
        (*if no dbname provided try to autodetect it*))
    | None, None -> (
        match find_file_repos () with
        | [ (name, root) ] -> (name, root)
        | [] -> raise (UsageError (Printf.sprintf "no file:// repository configured"))
        | _ ->
            raise
              (UsageError (Printf.sprintf "repository choice is ambiguous, more than one found")))
    | None, Some _ -> raise (UsageError "db_root provided without db_name")
  in
  let db_path =
    run_capture "realpath"
      [ "--"; db_root ^ "/" ^ db_name ^ "." ^ Option.value db_ext ~default:"db" ]
  in
  (db_name, db_root, db_path)

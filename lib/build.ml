open Utils
open Errors

let argv0 = "build"

let default_from_env x env_key =
  match x with Some _ -> x | None -> Sys.getenv_opt env_key

let check_readable_if_not_none = function
  | None -> ()
  | Some p ->
      if not (is_readable p) then
        raise (UsageError (Printf.sprintf "%s: %s: permission denied" argv0 p))

let strip_prefix prefix s =
  if String.starts_with ~prefix s then
    Some (String.sub s (String.length prefix) (String.length s - String.length prefix))
  else None

let resolve_db ?pacman_conf ?db_ext ?db_name () =
  let conf_args = match pacman_conf with Some p -> [ "--config"; p ] | None -> [] in
  let find_file_repos () =
    let repos =
      String.split_on_char '\n'
        (run_capture "pacman-conf" @@ conf_args @ [ "--repo-list" ])
    in
    List.filter_map
      (fun repo_name ->
        if repo_name = "" then None
        else
          let root =
            run_capture "pacman-conf" @@ conf_args @ [ "-r"; repo_name; "Server" ]
            |> String.split_on_char '\n' |> List.hd |> strip_prefix "file://"
          in
          (repo_name, root))
      repos
  in
  let db_name, db_root =
    match db_name with
    | Some name ->
        let root =
          let server =
            run_capture "pacman-conf" @@ conf_args @ [ "-r"; name; "Server" ]
            |> String.split_on_char '\n' |> List.hd
          in
          match strip_prefix "file://" server with
          | Some r -> r
          | None ->
              raise
                (UsageError
                   (Printf.sprintf "%s: %s: not a local repository" argv0 server))
        in
        (name, root)
    | None -> (
        match find_file_repos () with
        | [ (name, root) ] -> (name, root)
        | [] ->
            raise
              (UsageError (Printf.sprintf "%s: no file:// repository configured" argv0))
        | _ ->
            raise
              (UsageError
                 (Printf.sprintf
                    "%s: repository choice is ambiguous (use -d to specify)" argv0)))
  in
  let db_path =
    run_capture "realpath"
      [ "--"; db_root ^ "/" ^ db_name ^ "." ^ Option.value db_ext ~default:"db" ]
  in
  (db_name, db_root, db_path)

let main ?opt_db_ext ?opt_db_name ?pacman_conf ?makepkg_conf ?root ~chroot () =
  let db_ext = default_from_env opt_db_ext "AUR_DBEXT" in
  let db_name = default_from_env opt_db_name "AUR_REPO" in

  let makepkg_env =
    match Sys.getenv_opt "AUR_MAKEPKG_GNUPGHOME" with
    | Some v -> [ "GNUPGHOME=" ^ v ]
    | None -> []
  in

  let tmpdir = Option.value (Sys.getenv_opt "TMPDIR") ~default:"/var/tmp" in
  let aurutils_dir = tmpdir ^ "/aurutils" ^ string_of_int @@ Unix.getuid () in
  run_exn "mkdir" [ "-pm"; "0700"; "--"; aurutils_dir ];
  let var_tmp = Filename.temp_dir ~temp_dir:aurutils_dir (argv0 ^ ".") "" in

  Fun.protect
    ~finally:(fun () -> run_exn "rm" [ "rf"; "--"; var_tmp ])
    (fun () ->
      let pacman_conf, makepkg_conf =
        if chroot then
          let _, p, m =
            Chroot.get_default_paths_exn
              (run_capture "uname" [ "uname"; "-m" ])
              ?suffix:db_name ?pacman_conf ?makepkg_conf ()
          in
          (p, m)
        else (pacman_conf, makepkg_conf)
      in

      check_readable_if_not_none makepkg_conf;
      check_readable_if_not_none pacman_conf;

      let db_root, db_path = resolve_db ?pacman_conf ?db_ext ?db_name in
      ())

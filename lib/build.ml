open Utils

let argv0 = "build"

let default_from_env x env_key =
  match x with Some _ -> x | None -> Sys.getenv_opt env_key

let main ?opt_db_ext ?opt_db_name ?opt_db_root ~chroot () =
  let db_ext = default_from_env opt_db_ext "AUR_DBEXT" in
  let db_name = default_from_env opt_db_name "AUR_REPO" in
  let db_root = default_from_env opt_db_root "AUR_DBROOT" in

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
    ~finally:(fun () -> run_exn "rm" [ "rf"; "--"; "var_tmp" ])
    (fun () ->
      if chroot then
        let machine = run_capture "uname" [ "uname"; "-m" ] in
        let _, pacman_conf, makepkg_conf =
          Chroot.get_default_paths_exn machine ?suffix:db_name ()
        in

        ())

let command =
  Command.basic ~summary:"Build packages to a local repository"
    [%map_open.Command
      let sign_package =
        flag "-S" ~aliases:[ "--sign"; "--gpg-sign" ] no_arg
          ~doc:
            "Sign built packages and the database (repo-add -s) with gpg(1).\
             To use another key than the default, the GPGKEY environment\
             variable can be set to the appropriate key identifier.\
             Should the signing process fail for any reason, corresponding\
             packages are *not* moved to the local repository. aur-build will\
             instead exit with a diagnostic containing the package paths."
      and queuefile =
        flag "-a" ~aliases:[ "--arg-file" ] (optional string)
          ~doc:"FILE Read package directories from FILE (one per line)"
      and db_ext =
        flag "--dbext" (optional string) ~doc:"EXT Database archive extension"
      and db_name =
        flag "-d" ~aliases:[ "--database" ] (optional string)
          ~doc:"NAME Use NAME as the local repository"
      and pacman_conf =
        flag "-C" ~aliases:[ "--pacman-conf" ] (optional string)
          ~doc:"FILE pacman.conf file to use"
      and makepkg_conf =
        flag "-M" ~aliases:[ "--makepkg-conf" ] (optional string)
          ~doc:"FILE makepkg.conf file to use"
      and root =
        flag "--root" (optional string) ~doc:"DIR Repository root directory"
      and chroot =
        flag "-c" ~aliases:[ "--chroot" ] no_arg
          ~doc:"Build packages inside a chroot"
      and status =
        flag "--status" no_arg
          ~doc:"Print repository status (name, root, path) and exit"
      and results_file =
        flag "--results" (optional string)
          ~doc:"FILE Write build results to FILE"
      and results_append =
        flag "--results-append" no_arg
          ~doc:"Append to results file instead of truncating"
      and force =
        flag "-f" ~aliases:[ "--overwrite" ] no_arg
          ~doc:"Overwrite existing packages"
      and run_pkgver =
        flag "--pkgver" no_arg ~doc:"Run makepkg -od before the build"
      and ignorearch =
        flag "-A" ~aliases:[ "--ignorearch" ] no_arg ~doc:"Ignore architecture"
      and nocheck =
        flag "--nocheck" no_arg ~doc:"Do not run the check() function"
      and noconfirm =
        flag "--noconfirm" no_arg ~doc:"Pass --noconfirm to pacman"
      and rmdeps =
        flag "--rmdeps" no_arg ~doc:"Remove dependencies after a successful build"
      and syncdeps =
        flag "--syncdeps" no_arg ~doc:"Install missing dependencies"
      and bind_ro =
        flag "--bind" (listed string) ~doc:"DIR Bind DIR read-only to the chroot"
      and bind_rw =
        flag "--bind-rw" (listed string) ~doc:"DIR Bind DIR read-write to the chroot"
      and namcap =
        flag "-N" ~aliases:[ "--namcap" ] no_arg ~doc:"Run namcap on the built package"
      and checkpkg =
        flag "--checkpkg" no_arg ~doc:"Run checkpkg on the built package"
      and temp =
        flag "-T" ~aliases:[ "--temp" ] no_arg ~doc:"Use a temporary chroot"
      and user =
        flag "--user" (optional string) ~doc:"USER Build as USER in the chroot"
      and margs =
        flag "--margs" ~aliases:[ "--makepkg-args" ] (listed string)
          ~doc:"ARG Extra makepkg argument (repeatable)"
      and cargs =
        flag "--cargs" ~aliases:[ "--makechrootpkg-args" ] (listed string)
          ~doc:"ARG Extra makechrootpkg argument (repeatable)"
      and buildscript =
        flag "-p" (optional string) ~doc:"FILE Use FILE as the build script" in
      fun () ->
        match
          Aur.Build.main ~sign_package ?queuefile ?opt_db_ext:db_ext ?opt_db_name:db_name
            ?pacman_conf ?makepkg_conf ?root ~chroot ~status ?buildscript ?results_file
            ~results_append ~force ~run_pkgver ~ignorearch ~nocheck ~noconfirm ~rmdeps ~syncdeps
            ~bind_ro ~bind_rw ~namcap ~checkpkg ~temp ?user ~margs ~cargs ()
        with
        | () -> ()
        | exception (Aur.Errors.UsageError msg | Failure msg) ->
            Printf.eprintf "%s\n" msg;
            exit 1
        | exception Aur.Errors.SubExn (cmd, code) ->
            Printf.eprintf "%s: exited with error code %d\n" cmd code;
            exit 1
    ]

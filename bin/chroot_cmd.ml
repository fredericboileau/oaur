let command =
  Command.basic
    ~summary:"Manage containers for building packages"
    [%map_open.Command
       let pkgnames = anon (sequence ("pkgname" %: string)) and
       build = flag "-B" ~aliases:["--build"] no_arg ~doc:"
            Build a package inside the container with makechrootpkg.
            Assumes --create was run at least once" and
       update = flag "-U" ~aliases:["--update"] no_arg ~doc:"
            Update or create the /root copy of the container with arch-nspawn" and
       create = flag "--create" no_arg ~doc:"
            Create a new container with mkarchroot. By default the base-devel
            package group is installed to the container. If the hosrt architecture is x86_64
            and [multilib] is set in the pacman configuration (see --pacman-conf), the
            multilib-devel package group is also installed. If packages or package groups
            are ilsted on the command-line, these are installed instead of the above." and
       path = flag "--path" no_arg ~doc:"
            Print the path to the container template" and

       directory = flag "-D" ~aliases:["--directory"] (optional string) ~doc:"
            The base directory for containers. Defaults to /var/lib/aurbuild/<machine>.
            This directory usually contains a /root subdirectory that serves as a template for user containers (named after $SUDO_USER or /copy if unset).
            Note: If the -T parameter is specified to makechrootpkg, the user container has a random name and is removed on build completion." and
       bind_ro = flag "--bind" (listed string) ~doc:"
            Bind a directory read-only to the container" and
       bind_rw = flag "--bind-rw" (listed string) ~doc:"
            Bind a directory read-write to the container" and

       namcap = flag "-N" ~aliases:["--namcap"] no_arg ~doc:"" and
       checkpkg = flag "--checkpkg" no_arg ~doc:"" and
       temp = flag "-T" ~aliases:["--temp"] no_arg ~doc:"" and
       user = flag "--user" (optional string) ~doc:"" and

       ignorearch = flag "-A" ~aliases:["--ignorearch"] no_arg ~doc:"" and
       nocheck = flag "---nocheck" no_arg ~doc:"" and

       cargs = flag "--cargs" ~aliases:["--makechrootpkg-args"] (optional string) ~doc:"
             Arguments (comma-separated) to be passed to makchrootpkg for --build. Defaults to makechrootpkg -cu" and
       margs = flag "--margs" ~aliases:["--makepkg-args"] (optional string) ~doc:"
            Additional (comma-separated) makepkg arguments for makechrootpk. A default list of makepkg(8)
            arguments can be listed with makechrootpkg --help" and
       suffix = flag "--suffix" (optional string) ~doc:"SUFFIX suffix for pacman/makepkg config file lookup" and
       pacman_conf = flag "-C" ~aliases:["--pacman-conf"] (optional string) ~doc:"FILE pacman.conf file used inside the container" and
       makepkg_conf = flag "-M" ~aliases:["--makepkg-conf"] (optional string) ~doc:"FILE makepkg.conf file used inside the container" in

       let args_from_opt lst = List.filter_map (fun (b, s) -> if b then Some s else None) lst in
       let split_opt = function Some s -> String.split_on_char ',' s | None -> [] in

       let makechrootpkg_args =
         "-cu"
         :: split_opt cargs
         @ args_from_opt [(namcap, "-n"); (checkpkg, "-C"); (temp, "-T")]
         @ (match user with Some u -> ["-U"; u] | None -> [])
       in
       let makechrootpkg_makepkg_args =
         split_opt margs
         @ args_from_opt [(ignorearch, "--ignorearch"); (nocheck, "--nocheck")]
       in

       fun () ->
         match Aur.Chroot.chroot
                 ?suffix ?pacman_conf ?makepkg_conf
                 ?directory
                 ~build ~update ~create ~path
                 ~bind_ro ~bind_rw
                 ~pkgnames
                 ~makechrootpkg_args
                 ~makechrootpkg_makepkg_args
                 ()
         with
         | () -> ()
         | exception (Aur.Errors.UsageError msg | Failure msg | Aur.Errors.SubExn msg) ->
             Printf.eprintf "%s\n" msg;
             exit 1
    ]

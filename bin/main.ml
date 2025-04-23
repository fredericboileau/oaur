open Core

let search = 
    Command.basic 
        ~summary:"Search for aur package" 
        ~readme:(fun () -> "More detailed information")
        [%map_open.Command
            let term = anon ("term" %: string)
            and sort_criteria = anon (maybe ("sort_criteria" %: string)) in
                fun () -> Lwt_main.run (Aur.search term)] 
let depends = 
  Command.basic
      ~summary:"Get dependencies of pkg"
      ~readme:(fun () -> "More detailed information")
      [%map_open.Command
       let pkgname = anon ("pkgname" %: string) in
           fun () -> Lwt_main.run (Aur.depends pkgname)]

let fetch = 
  Command.basic 
      ~summary:"Fetch aur package"
      [%map_open.Command
       let pkgnames  = anon (sequence ("pkgname" %: string)) and
           syncmode = flag "--syncmode" (optional string)
                          ~doc:"syncmode when fetching aur repo" and
           discard = flag "--discard" no_arg
                         ~doc:"discard local changes when syncing" in
           fun () -> Aur.fetch pkgnames syncmode discard]

let chroot =
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

           ignorearch = flag "-A" ~aliases:["--ignorearch"]  no_arg ~doc:"" and
           nocheck = flag "---nocheck" no_arg ~doc:"" and

           cargs = flag "--cargs" ~aliases:["--makechrootpkg-args"] (optional string) ~doc:"
                 Arguments (comma-separated) to be passed to makchrootpkg for --build. Defaults to makechrootpkg -cu" and
           margs = flag "--margs" ~aliases:["--makepkg-args"] (optional string) ~doc:"
                Additional (comma-separated) makepkg arguments for makechrootpk. A default list of makepkg(8)
                arguments can be listed with makechrootpkg --help" in

           let rec args_translate lst result =
             match lst with
             | (true, translation)::tl -> args_translate tl (translation::result)
             | (false, _)::tl -> args_translate tl result
             | [] -> List.rev result
           in
           let makechrootpkg_makepkg_args_translation =
               [(ignorearch, "--ignorearch"); (nocheck, "--nocheck")] in

           let makechrootpkg_arg_translation =
             [(namcap, "-n"); (checkpkg,"-C"); (temp, "-T")] in

           let makechrootpkg_args_init = "-cu" ::
                                           match cargs with
                                           | Some str -> (String.split ~on:',' str)
                                           | None -> []
           in
           let makechrootpkg_makepgkg_args_init = match margs with
             | Some str -> String.split ~on:',' str
             | None -> []
           in

           let makechrootpkg_args = args_translate
                                        makechrootpkg_arg_translation
                                        makechrootpkg_args_init in
           let makechrootpkg_args = match user with
             | Some user -> List.append makechrootpkg_args ["-U"; user]
             | None -> makechrootpkg_args
           in
           let makechrootpkg_makepkg_args = args_translate
                                                makechrootpkg_makepkg_args_translation
                                                makechrootpkg_makepgkg_args_init in

           fun () -> Aur.chroot
                         build update create path
                         bind_ro bind_rw
                         pkgnames
                         makechrootpkg_args
                         makechrootpkg_makepkg_args
        ]
 
           


let command = 
    Command.group
        ~summary:"aur utilities clone of aurutils" 
        ~readme:(fun () -> "oaur is the command wrapper for oaurutils.")
        [("search", search); ("depends", depends); ("fetch", fetch); ("chroot", chroot)]


let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

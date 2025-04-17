let command = 
    let open Core in
    let search = 
        Command.basic 
            ~summary:"Search for aur package" 
            ~readme:(fun () -> "More detailed information")
            [%map_open.Command
                let term = anon ("term" %: string)
                and sort_criteria = anon (maybe ("sort_criteria" %: string)) in
                    fun () -> Lwt_main.run (Aur.search term)] 
    in
    let depends = 
        Command.basic
           ~summary:"Get dependencies of pkg"
           ~readme:(fun () -> "More detailed information")
           [%map_open.Command
                let pkgname = anon ("pkgname" %: string) in
                fun () -> Lwt_main.run (Aur.depends pkgname)]
    in
    let fetch = 
        Command.basic 
            ~summary:"Fetch aur package"
            [%map_open.Command
                let pkgnames  = anon (sequence ("pkgname" %: string)) and
                syncmode = flag "--syncmode" (optional string)
                                ~doc:"syncmode when fetching aur repo" and
                discard = flag "--discard" no_arg
                                ~doc:"discard local changes when syncing" in
                fun () -> Lwt_main.run (Aur.fetch pkgnames syncmode discard)]
    in
    let chroot =
      Command.basic
        ~summary:"Manage containers for building packages"
        [%map_open.Command
           let pkgnames = anon (sequence ("pkgname" %: string)) and
           build = flag "-B" ~aliases:["--build"] no_arg
                    ~doc:"Build a package inside the container with makechrootpkg.
                         Assumes --create was run at least once" and
           update = flag "-U" ~aliases:["--update"] no_arg ~doc:"
                Update or create the /root copy of the container with arch-nspawn" and
           create = flag "--create" no_arg ~doc:"
                Create a new container with mkarchroot. By default the base-devel
                package group is installed to the container. If the hosrt architecture is x86_64
                and [multilib] is set in the pacman configuration (see --pacman-conf), the
                multilib-devel package group is also installed. If packages or package groups
                are ilsted on the command-line, these are installed instead of the above." and
           path = flag "--path" no_arg ~doc:"Print the path to the container template" in
           fun () -> (Aur.chroot build update create path pkgnames)
        ]

    in
    Command.group 
        ~summary:"aur utilities clone of aurutils" 
        ~readme:(fun () -> "lorem")
        [("search", search); ("depends", depends);
         ("fetch", fetch); ("chroot", chroot)]
                    


let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

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
                let pkgname  = anon ("pkgname" %: string) and
                syncmode = flag "--syncmode" (optional string)
                                ~doc:"syncmode when fetching aur repo" and
                discard = flag "--discard" no_arg
                                ~doc:"discard local changes when syncing" in
                fun () -> Lwt_main.run (Aur.fetch pkgname syncmode discard)]
    in
    Command.group 
        ~summary:"aur utilities clone of aurutils" 
        ~readme:(fun () -> "lorem")
        [("search", search); ("depends", depends); ("fetch", fetch)]
                    


let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

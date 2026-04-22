open Core

let mode_of_string mode_as_string = 
  let open Aur.Depends in
  match mode_as_string with
  | "pairs" -> Pairs
  | "table" -> Table
  | "json" -> Json
  | "jsonl" -> JsonLines
  | s -> failwith ("unknown mode: " ^ s)

let search = 
    Command.basic 
        ~summary:"Search for aur package" 
        ~readme:(fun () -> "More detailed information")
        [%map_open.Command
            let term = anon ("term" %: string)
            (* and sort_criteria = anon (maybe ("sort_criteria" %: string)) in *)
            in
                fun () -> Lwt_main.run (Aur.Commands.search term)] 

let depends = 
  Command.basic
      ~summary:"Get dependencies of pkg"
      ~readme:(fun () -> "More detailed information")
      [%map_open.Command
       let targets = anon (sequence ("pkgname" %: string)) and
       output_mode = flag "--output-mode" (optional_with_default "pairs" string) ~doc:"MODE pairs|table|json|jsonl" and
       no_depends = flag "--no-depends" no_arg ~doc:"don't include depends" and
       no_makedepends = flag "--no-makedepends" no_arg ~doc:"don't include makedepends" and
       no_checkdepends = flag "--no-checkdepends" no_arg ~doc:"don't include checkdepends" and
       optdepends = flag "--optdepends" no_arg ~doc:"include optdepends"and
       opt_reverse = flag "--reverse" no_arg ~doc:"reverse dependency" and
       assume_installed = flag "--assume-installed" (optional string) ~doc:"selectively remove dependencies, separate packages with a comma" and
       no_provides = flag "--no-provides" no_arg ~doc:"Disable support for virutall dependencies" and
       pkgname = flag "--pkgname" no_arg ~doc:"print package name instead of package base" and
       verify = flag "--verify" no_arg ~doc:"verify versions" in
           fun () -> Lwt_main.run (
             Aur.Depends.main 
              targets
              ~output_mode:(mode_of_string output_mode) 
              ~opt_reverse
              ~include_depends:(not no_depends)
              ~include_makedepends:(not no_makedepends)
              ~include_checkdepends:(not no_checkdepends)
              ~include_optdepends:optdepends
              ~verify
              ~installed:(Option.value_map assume_installed ~default:[] ~f:(String.split ~on:','))
              ~provides:(not no_provides)
              ~opt_pkgname:pkgname
            )
        ]

let fetch = 
  Command.basic 
      ~summary:"Fetch aur package"
      [%map_open.Command
       let pkgnames  = anon (sequence ("pkgname" %: string)) and
           syncmode = flag "--syncmode" (optional string)
                          ~doc:"syncmode when fetching aur repo" and
           discard = flag "--discard" no_arg
                         ~doc:"discard local changes when syncing" in
           fun () -> Aur.Commands.fetch pkgnames syncmode discard]

let command =
    Command.group
        ~summary:"aur utilities clone of aurutils" 
        ~readme:(fun () -> "oaur is the command wrapper for oaurutils.")
        [("search", search); ("depends", depends); ("fetch", fetch); 
        ("chroot", Chroot_cmd.command); ("build", Build_cmd.command)]


let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

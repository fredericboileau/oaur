open Errors
open Utils

let ( // ) = Filename.concat
let directory_exists d = Sys.file_exists d && Sys.is_directory d
let is_regular_file p = try Sys.is_regular_file p with Sys_error _ -> false

let bindmounts_rw_from_conf pacman_conf =
  let r = Str.regexp {|Server = file://\(.*\)|} in
  let inp =
    Unix.open_process_args_in "pacman-conf"
      [| "pacman-conf"; "--config"; pacman_conf |]
  in
  In_channel.input_lines inp
  |> List.filter_map (fun line ->
      if Str.string_match r line 0 then Some (Str.matched_group 1 line)
      else None)

let chroot ?suffix ?pacman_conf ?makepkg_conf build update create path opt_directory opt_bind_ro
    opt_bind_rw pkgnames makechrootpkg_args makechrootpkg_makepkg_args =
  (*TODO eventually change interface to --action, mutually exclusive in definition*)
  let count_action_requested =
    List.fold_left
      (fun acc x -> if x then acc + 1 else acc)
      0
      [ build; update; create; path ]
  in
  if count_action_requested <> 1 then
    raise
      (UsageError
         "exactly one of --build, --update, --create, --path must be specified");

  let machine = String.trim (run_read_all ("uname", [ "uname"; "-m" ])) in
  let etcdir = "/etc/aurutils" in
  let shrdir = "/usr/share/devtools" in
  let directory =
    Option.value ~default:("/var/lib/aurbuild" // machine) opt_directory
  in
  let default_pacman_paths =
    List.filter_map Fun.id
      [
        Option.map (fun s -> (etcdir // "pacman-") ^ s ^ ".conf") suffix;
        Some ((etcdir // "pacman-") ^ machine ^ ".conf");
        Option.map (fun s -> (shrdir // "pacman.conf.d" // s) ^ ".conf") suffix;
        Some ((shrdir // "pacman.conf.d" // "aurutils-") ^ machine ^ ".conf");
      ]
  in
  let default_makepkg_paths =
    List.filter_map Fun.id
      [
        Option.map (fun s -> (etcdir // "makepkg-") ^ s ^ ".conf") suffix;
        Some ((etcdir // "makepkg-") ^ machine ^ ".conf");
        Option.map (fun s -> (shrdir // "makepkg.conf.d" // s) ^ ".conf") suffix;
        Some ((shrdir // "makepkg.conf.d" // machine) ^ ".conf");
      ]
  in

  let diag_conf kind paths =
    Printf.eprintf "chroot: %s configuration not found, looked in:\n" kind;
    List.iter (Printf.eprintf "  %s\n") paths;
    exit 2
  in
  let resolve_conf kind opt default_paths =
    match opt with
    | Some p when is_regular_file p -> p
    | Some p ->
        Printf.eprintf "chroot: %s configuration not found: %s\n" kind p;
        exit 2
    | None ->
        match List.find_opt is_regular_file default_paths with
        | Some p -> p
        | None -> diag_conf kind default_paths
  in
  let pacman_conf = resolve_conf "pacman" pacman_conf default_pacman_paths in
  let makepkg_conf = resolve_conf "makepkg" makepkg_conf default_makepkg_paths in

  if create then begin
    (*if packages listed use those, otherwise install base-devel and multilib-devel if multilib present in pacman config*)
    let base_packages =
      if pkgnames <> [] then pkgnames
      else if
        run_read_all ("pacini", [ "pacini"; "--section=multilib"; pacman_conf ])
        <> ""
        && machine = "x86_64"
      then [ "base-devel"; "multilib-devel" ]
      else [ "base-devel" ]
    in
    if not (directory_exists directory) then
      run ("sudo", [ "sudo"; "install"; "-d"; directory; "-m"; "755"; "-v" ]);

    if not (directory_exists (directory // "root")) then
      run ~suppress_output:false
        ( "sudo",
          [
            "sudo";
            "mkarchroot";
            "-C";
            pacman_conf;
            "-M";
            makepkg_conf;
            directory // "root";
            String.concat " " base_packages;
          ] )
  end
  else begin
    (*when not create we need root to exist*)
    if not (directory_exists (directory // "root")) then
      raise
        (Failure
           (Printf.sprintf
              "chroot: %S is not a directory\n\
               chroot: did you run aur chroot --create"
              (directory // "root")));

    (*update and build need bindmounts from file sections in pacmanconf*)
    let pacman_conf_bindmounts_rw = bindmounts_rw_from_conf pacman_conf in

    if update then
      let bind_ro = List.map (fun b -> "--bind-ro=" ^ b) opt_bind_ro in
      let bind_rw =
        List.map
          (fun b -> "--bind=" ^ b)
          (opt_bind_rw @ pacman_conf_bindmounts_rw)
      in
      run
        ( "sudo",
          [
            "sudo";
            "arch-nspawn";
            "-C";
            pacman_conf;
            "-M";
            makepkg_conf;
            directory // "root";
          ]
          @ bind_rw @ bind_ro
          @ [ "pacman"; "-Syu"; "--noconfirm" ]
          @ pkgnames )
    else if build then
      let bind_ro = List.map (fun b -> "-D" ^ b) opt_bind_ro in
      let bind_rw = List.map (fun b -> "-d" ^ b) pacman_conf_bindmounts_rw in
      run
        ( "sudo",
          [
            "sudo";
            "--preserve-env=SRCDEST,SRCPKGDEST,PKGDEST,LOGDEST,MAKEFLAGS,PACKAGER,GNUPGHOME,SSH_AUTH_SOCK";
            "makechrootpkg";
            "-r";
            directory;
          ]
          @ bind_ro @ bind_rw @ makechrootpkg_args @ [ "--" ]
          @ makechrootpkg_makepkg_args )
    else if path then
      (* if path *)
      let realpath =
        String.trim
          (run_read_all ("realpath", [ "realpath"; "--"; directory // "root" ]))
      in

      print_endline realpath
    else assert false
  end

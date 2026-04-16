open Errors
open Utils

let ( // ) = Filename.concat
let directory_exists d = Sys.file_exists d && Sys.is_directory d
let is_regular_file p = try Sys.is_regular_file p with Sys_error _ -> false

let get_default_paths_exn machine ?suffix ?directory ?pacman_conf ?makepkg_conf
    () =
  let etcdir = "/etc/aurutils" in
  let shrdir = "/usr/share/devtools" in
  let directory =
    Option.value ~default:("/var/lib/aurbuild" // machine) directory
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
  let resolve kind opt paths =
    match opt with
    | Some p when is_regular_file p -> Some p
    | Some p ->
        raise
          (UsageError
             (Printf.sprintf "chroot: %s configuration not found: %s" kind p))
    | None -> List.find_opt is_regular_file paths
  in
  ( directory,
    resolve "pacman" pacman_conf default_pacman_paths,
    resolve "makepkg" makepkg_conf default_makepkg_paths )

let require_conf machine kind = function
  | Some p -> p
  | None ->
      let template =
        match kind with
        | "pacman" -> "/usr/share/devtools/pacman.conf.d/extra.conf"
        | "makepkg" -> "/usr/share/devtools/makepkg.conf.d/x86_64.conf"
        | _ -> assert false
      in
      raise
        (UsageError
           (Printf.sprintf
              ("chroot: %s configuration not found\n"
             ^^ "chroot: consider copying a template, e.g.:\n"
             ^^ "cp %s /etc/aurutils/%s-%s.conf")
              kind template kind machine))

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

let chroot ~build ~update ~create ~path ?directory ~bind_ro:opt_bind_ro
    ~bind_rw:opt_bind_rw ~pkgnames ~makechrootpkg_args
    ~makechrootpkg_makepkg_args ?suffix ?pacman_conf ?makepkg_conf () =
  let machine = run_capture "uname" [ "uname"; "-m" ] in
  let directory, pacman_conf_opt, makepkg_conf_opt =
    get_default_paths_exn machine ?suffix ?directory ?pacman_conf ?makepkg_conf
      ()
  in
  let pacman_conf = require_conf machine "pacman" pacman_conf_opt in
  let makepkg_conf = require_conf machine "makepkg" makepkg_conf_opt in

  let count_action_requested =
    List.fold_left
      (fun acc x -> if x then acc + 1 else acc)
      0
      [ build; update; create; path ]
  in
  if count_action_requested > 1 then
    raise
      (UsageError
         "exactly one of --build, --update, --create, --path must be specified")
  else if count_action_requested = 0 then
    Printf.printf "chroot:%s\npacman:%s\nmakepkg:%s\n" directory pacman_conf
      makepkg_conf
  else begin
    if create then begin
      (*if packages listed use those, otherwise install base-devel and multilib-devel if multilib present in pacman config*)
      let base_packages =
        if pkgnames <> [] then pkgnames
        else if
          run_capture "pacini" [ "pacini"; "--section=multilib"; pacman_conf ]
          <> ""
          && machine = "x86_64"
        then [ "base-devel"; "multilib-devel" ]
        else [ "base-devel" ]
      in
      if not (directory_exists directory) then
        run_exn "sudo" [ "install"; "-d"; directory; "-m"; "755"; "-v" ];

      if not (directory_exists (directory // "root")) then
        run_exn "sudo"
          [
            "mkarchroot";
            "-C";
            pacman_conf;
            "-M";
            makepkg_conf;
            directory // "root";
            String.concat " " base_packages;
          ]
    end
    else begin
      (*when not create we need root to exist*)
      if not (directory_exists (directory // "root")) then
        raise
          (Failure
             (Printf.sprintf
                "chroot: %S is not a directory\nchroot: did you run aur chroot --create"
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
        run_exn "sudo"
          ([
             "arch-nspawn";
             "-C";
             pacman_conf;
             "-M";
             makepkg_conf;
             directory // "root";
           ]
          @ bind_rw @ bind_ro
          @ [ "pacman"; "-Syu"; "--noconfirm" ]
          @ pkgnames)
      else if build then
        let bind_ro = List.map (fun b -> "-D" ^ b) opt_bind_ro in
        let bind_rw = List.map (fun b -> "-d" ^ b) pacman_conf_bindmounts_rw in
        run_exn "sudo"
          ([
             "--preserve-env=SRCDEST,SRCPKGDEST,PKGDEST,LOGDEST,MAKEFLAGS,PACKAGER,GNUPGHOME,SSH_AUTH_SOCK";
             "makechrootpkg";
             "-r";
             directory;
           ]
          @ bind_ro @ bind_rw @ makechrootpkg_args @ [ "--" ]
          @ makechrootpkg_makepkg_args)
      else if path then
        (* if path *)
        let realpath = run_capture "realpath" [ "--"; directory // "root" ] in

        print_endline realpath
      else assert false
    end
  end

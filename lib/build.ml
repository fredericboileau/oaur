open Utils
open Errors
open Core

let argv0 = "build"
let default_from_env x env_key = match x with Some _ -> x | None -> Sys.getenv env_key
let default_gpg_args = [ "--detach-sign"; "--no-armor"; "--batch" ]
let prefix dir lst = List.map lst ~f:(Filename.concat dir)

let check_readable_if_not_none = function
  | None -> ()
  | Some p ->
      if not (is_readable p) then
        raise (UsageError (Printf.sprintf "%s: %s: permission denied" argv0 p))

(*TODO*)
let diag_moved_packages dir =
  let eprint_indented n msglines =
    let pad = String.make n ' ' in
    List.iter msglines ~f:(fun line -> Printf.eprintf "%s%s\n" pad line)
  in
  eprint_indented 4
    [
      "aur-build encountered an error before moving packages to the local repository";
      "This may happend when signing built packages with gpg (aur build --sign),";
      "or with certain makepkg errors\n";
      "The following files were preserved";
    ];
  let files = Sys_unix.readdir dir |> Array.to_list |> List.map ~f:(Filename.concat dir) in
  eprint_indented 8 files

let existing_pkglist_if_all ?makepkg_conf ?results_file ~db_root () =
  let pkglist =
    let env = Array.append (Core_unix.environment ()) [| "PKGDEST=" ^ db_root |] in
    let makepkg_args =
      [ "--packagelist" ]
      @ Option.value_map makepkg_conf ~default:[] ~f:(fun c -> [ "--config"; c ])
    in
    run_capture "makepkg" makepkg_args ~env
    |> String.split ~on:'\n'
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let existing_pkglist =
    List.filter pkglist ~f:(fun f ->
        match Sys_unix.file_exists f with `Yes -> true | `No | `Unknown -> false)
  in
  let all_exist = List.length existing_pkglist = List.length pkglist in
  if all_exist then begin
    Printf.eprintf "%s: warning: skipping existing package (use -f to overwrite)\n" argv0;
    List.iter existing_pkglist ~f:(fun p -> Printf.eprintf "%S\n" p)
  end
  else if not (List.is_empty existing_pkglist) then begin
    (*Since makepkg does not allow building split packages individually, we
    restart the build when part of the package group is unavailable*)
    Printf.eprintf "%s: warning: package group partially built\n" argv0;
    List.iter existing_pkglist ~f:(fun p -> Printf.eprintf "%S\n" p)
  end;

  if not (List.is_empty existing_pkglist) then
    Option.iter results_file ~f:(fun path ->
        Out_channel.with_file ~append:true path ~f:(fun oc ->
            List.iter existing_pkglist ~f:(Out_channel.fprintf oc "exist:file://%s\n")));

  if all_exist then Some existing_pkglist else None

let main ~sign_package ?queuefile ?opt_db_ext ?opt_db_name ?pacman_conf ?makepkg_conf ?root
    ~chroot ~status ?buildscript ?results_file ?(results_append = false) ?(run_pkgver = false)
    ?(ignorearch = false) ?(nocheck = false) ?(noconfirm = false) ?(rmdeps = false)
    ?(syncdeps = false) ?(force = false) ?(bind_ro = []) ?(bind_rw = []) ?(namcap = false)
    ?(checkpkg = false) ?(temp = false) ?user ?(margs = []) ?(cargs = []) ?db_pool
    ?(verify = false) ?(remove = false) ?(new_only = false) ?(prevent_downgrade = false)
    ?(nosync = false) () =
  let db_ext = default_from_env opt_db_ext "AUR_DBEXT" in
  let db_name = default_from_env opt_db_name "AUR_REPO" in

  let makepkg_env =
    match Sys.getenv "AUR_MAKEPKG_GNUPGHOME" with Some v -> [ "GNUPGHOME=" ^ v ] | None -> []
  in

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

  let db_name, db_root, db_path = Repo.resolve_db ?pacman_conf ?db_ext ?db_name () in

  if Option.is_none db_ext && String.is_suffix ~suffix:".db" db_path then
    raise
      (UsageError (Printf.sprintf "%s does not have a valid database archive extension" db_path));

  if not (is_readable db_path) then
    raise (UsageError (Printf.sprintf "%s is not a regular file" db_path));

  if status then begin
    Printf.printf "repo:%s\nroot:%s\npath:%s" db_name db_root db_path;
    exit 0
  end;

  let db_writable =
    match Core_unix.access db_path [ `Write ] with Ok () -> true | Error _ -> false
  in
  if not db_writable then raise (UsageError (Printf.sprintf "%s permission denied" db_path));

  let results_file =
    Option.map
      ~f:(fun p ->
        let p = run_capture "realpath" [ "--"; p ] in
        (*truncate file by default*)
        if not results_append then Out_channel.write_all p ~data:"";
        p)
      results_file
  in

  let pkgdirs =
    match queuefile with Some f -> In_channel.read_lines f | None -> [ Sys_unix.getcwd () ]
  in

  let db_sigs =
    [ [%string "%{db_root}/%{db_name}.sig"]; [%string "%{db_root}/%{db_name}.files.sig"] ]
  in
  if (not sign_package) && file_exists (List.hd_exn db_sigs) then begin
    let db_sigs_str = String.concat ~sep:"\n" db_sigs in
    raise
      (UsageError
         ([%string "%{argv0}: database signature found, but signing is disabled\n"] ^ db_sigs_str))
  end;

  (*verify key*)
  let gpgkey = Sys.getenv "GPGKEY" in
  (match gpgkey with Some key -> run_exn "gpg" [ "--list-keys"; key ] | None -> ());
  let gpg_args =
    match gpgkey with Some key -> default_gpg_args @ [ "-u"; key ] | None -> default_gpg_args
  in

  if chroot then
    Chroot.chroot ~build:false ~update:true ~create:true ~path:false ?directory:root
      ~suffix:db_name ?pacman_conf ?makepkg_conf ();

  let repo_add_args =
    args_from_boolean
      [
        (sign_package, "-s");
        (verify, "-v");
        (remove, "-R");
        (new_only, "-n");
        (prevent_downgrade, "-p");
      ]
  in
  let makepkg_common_args =
    Option.value_map makepkg_conf ~default:[] ~f:(fun c -> [ "--config"; c ])
    @ Option.value_map buildscript ~default:[] ~f:(fun b -> [ "-p"; b ])
    @ args_from_boolean
        [
          (ignorearch, "--ignorearch");
          (nocheck, "--nocheck");
          (noconfirm, "--noconfirm");
          (rmdeps, "--rmdeps");
          (syncdeps, "--syncdeps");
        ]
  in
  let tmpdir = Option.value (Sys.getenv "TMPDIR") ~default:"/var/tmp" in
  let aurutils_dir = tmpdir ^ "/aurutils" ^ string_of_int @@ Core_unix.getuid () in
  run_exn "mkdir" [ "-pm"; "0700"; "--"; aurutils_dir ];
  let var_tmp = Filename_unix.temp_dir ~in_dir:aurutils_dir (argv0 ^ ".") "" in

  Fun.protect
    ~finally:(fun () ->
      let status = run "rm" [ "-df"; "--"; var_tmp ] in
      match status with WEXITED 0 -> () | _ -> diag_moved_packages var_tmp)
    (fun () ->
      let startdir = Sys_unix.getcwd () in
      List.iter pkgdirs ~f:(fun pkg ->
          Sys_unix.chdir startdir;
          Sys_unix.chdir pkg;
          if not (file_exists "PKGBUILD") then
            raise (UsageError [%string "PKGBUILD does not exist for %{pkg}"])
          else begin
            (*update PKGBUILD version*)
            if run_pkgver then run_exn "makepkg" ("-od" :: makepkg_common_args);
            let create_package, pre_pkglist =
              if force then (true, [])
              else
                (*only consider prebuilt if all are built otherwise create fresh*)
                (*do_all_exist returns None if not all exist*)
                match existing_pkglist_if_all ?makepkg_conf ?results_file ~db_root () with
                | Some existing -> (false, existing)
                | None -> (true, [])
            in
            if create_package then begin
              let env =
                Array.append (Core_unix.environment ())
                @@ Array.of_list (("PKGDEST=" ^ var_tmp) :: makepkg_env)
              in
              if chroot then
                let makechrootpkg_args =
                  ("-cu" :: cargs)
                  @ args_from_boolean [ (namcap, "-n"); (checkpkg, "-C"); (temp, "-T") ]
                  @ Option.value_map user ~default:[] ~f:(fun u -> [ "-U"; u ])
                in
                let makechrootpkg_makepkg_args =
                  margs
                  @ args_from_boolean [ (ignorearch, "--ignorearch"); (nocheck, "--nocheck") ]
                  @ if run_pkgver then [ "--holdver" ] else []
                in
                Chroot.chroot ~build:true ~update:false ~create:false ~path:false ~env ~bind_ro
                  ~bind_rw ~makechrootpkg_args ~makechrootpkg_makepkg_args ?directory:root
                  ~suffix:db_name ?pacman_conf ?makepkg_conf ()
              else run_exn ~env "makepkg" @@ makepkg_common_args @ margs
            end;
            let pkglist =
              if create_package then
                Sys_unix.readdir var_tmp |> Array.to_list
                |> List.filter ~f:(fun f -> not (String.is_suffix f ~suffix:".sig"))
              else List.map pre_pkglist ~f:Filename.basename
            in
            let siglist =
              List.filter_map pkglist ~f:(fun pkg ->
                  let sig_name = [%string "%{pkg}.sig"] in
                  (*signature from makepkg --sign*)
                  if file_exists (Filename.concat var_tmp sig_name) then Some sig_name
                  else if
                    (*skipped package build with signature*)
                    file_exists [%string "%{db_root}/%{sig_name}"]
                    && not (file_exists (Filename.concat var_tmp pkg))
                  then begin
                    prerr_endline
                      [%string "%{argv0}: existing signature file %{db_root}/%{sig_name}"];
                    None
                  end
                  else if sign_package then begin
                    (*no candidate signature, generate one*)
                    let pkg_src =
                      if create_package then Filename.concat var_tmp pkg
                      else Filename.concat db_root pkg
                    in
                    run_exn "gpg" @@ gpg_args
                    @ [ "--output"; Filename.concat var_tmp sig_name; pkg_src ];
                    Some sig_name
                  end
                  (*signing disabled, no local sig file, no sig in db_root or the package exists locally*)
                    else None)
            in
            if create_package then begin
              (match db_pool with
              | Some pool ->
                  run_exn "mv" @@ [ "-f" ] @ prefix var_tmp siglist @ prefix var_tmp pkglist
                  @ [ pool ];
                  run_exn "ln" @@ [ "-st"; db_root; "--" ] @ prefix pool pkglist
                  @ prefix pool siglist
              | None ->
                  run_exn "mv"
                    ([ "-f" ] @ prefix var_tmp siglist @ prefix var_tmp pkglist @ [ db_root ]));
              Option.iter results_file ~f:(fun path ->
                  Out_channel.with_file ~append:true path ~f:(fun oc ->
                      List.iter pkglist ~f:(fun p ->
                          Out_channel.fprintf oc "build:file://%s\n" (Filename.concat db_root p))))
            end
            else if not (List.is_empty siglist) then
              run_exn "mv" ([ "-f" ] @ prefix var_tmp siglist @ [ db_root ]);
            Sys_unix.chdir db_root;
            let env = Array.append (Core_unix.environment ()) [| "LANG=C" |] in
            run_exn ~env "repo-add" @@ repo_add_args @ [ db_path ] @ pkglist;
            Sys_unix.chdir startdir;

            if (not chroot) && not nosync then
              let auth = Option.value (Sys.getenv "AUR_PACMAN_AUTH") ~default:"sudo" in
              let sync_args =
                [ "-d"; db_name; "--sysupgrade" ]
                @ args_from_boolean [ (noconfirm, "--noconfirm") ]
                @ Option.value_map pacman_conf ~default:[] ~f:(fun c -> [ "--config"; c ])
              in
              run_exn auth ("oaur" :: "build-sync" :: sync_args)
          end))

let sync ~db_names ?pacman_conf ~noconfirm ~sysupgrade () =
  let get_local_config conf_args db_names =
    let global_options = run_capture "pacconf" @@ conf_args @ [ "--options"; "--raw" ] in
    let repo_options =
      List.map db_names ~f:(fun repo ->
          Printf.sprintf "[%s]\n%s" repo
            (run_capture "pacconf" @@ conf_args @ [ "--repo"; repo; "--raw" ]))
    in
    String.concat ~sep:"\n" ("[options]" :: global_options :: repo_options)
  in
  let waitlock lockfile =
    while file_exists lockfile do
      let timer = ref 0 in
      Printf.eprintf "%s: pacman is currently in use, please wait...\n" argv0;
      while file_exists lockfile && !timer < 10 do
        timer := !timer + 1;
        Core_unix.sleep 3
      done
    done
  in
  let conf_args = Option.value_map pacman_conf ~default:[] ~f:(fun c -> [ "--config"; c ]) in
  let lockfile = run_capture "packlock" (conf_args @ [ "--print" ]) in
  waitlock lockfile;
  run_exn "pacsync" (conf_args @ db_names);
  waitlock lockfile;
  run_exn "pacsync" (conf_args @ db_names @ [ "--dbext=.files" ]);
  let sync_args =
    args_from_boolean [ (sysupgrade, "--sysupgrade"); (noconfirm, "--noconfirm") ]
  in
  if not (List.is_empty db_names) then
    let sync_args = sync_args @ [ "--resolve-replacements=none" ] in
    let tmp = Filename_unix.temp_file "oaur" ".conf" in
    Fun.protect
      ~finally:(fun () -> Sys_unix.remove tmp)
      (fun () ->
        Out_channel.write_all tmp ~data:(get_local_config conf_args db_names);
        waitlock lockfile;
        run_exn "pactrans" @@ sync_args @ [ "--config"; tmp ])
  else begin
    waitlock lockfile;
    run_exn "pactrans" @@ sync_args @ conf_args
  end

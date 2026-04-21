open Utils
open Errors
open Core

let argv0 = "build"
let default_from_env x env_key = match x with Some _ -> x | None -> Sys.getenv env_key
let default_gpg_args = [ "--detach-sign"; "--no-armor"; "--batch" ]

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

let main ~sign_package ?queuefile ?opt_db_ext ?opt_db_name ?pacman_conf ?makepkg_conf ?root
    ~chroot ~status ?buildscript ?results_file ?(results_append = false) ?(run_pkgver = false)
    ?(ignorearch = false) ?(noconfirm = false) ?(rmdeps = false) ?(syncdeps = false)
    ?(force = false) () =
  let db_ext = default_from_env opt_db_ext "AUR_DBEXT" in
  let db_name = default_from_env opt_db_name "AUR_REPO" in

  let makepkg_env =
    match Sys.getenv "AUR_MAKEPKG_GNUPGHOME" with Some v -> [ "GNUPGHOME=" ^ v ] | None -> []
  in

  (* let tmpdir = Option.value (Sys.getenv "TMPDIR") ~default:"/var/tmp" in *)
  (* let aurutils_dir = tmpdir ^ "/aurutils" ^ string_of_int @@ Core_unix.getuid () in *)
  (* run_exn "mkdir" [ "-pm"; "0700"; "--"; aurutils_dir ]; *)
  (* let var_tmp = Filename_unix.temp_dir ~in_dir:aurutils_dir (argv0 ^ ".") "" in *)

  (* Fun.protect *)
  (*   ~finally:(fun () -> *)
  (*     let status = run "rm" [ "-df"; "--"; var_tmp ] in *)
  (*     match status with WEXITED 0 -> () | _ -> diag_moved_packages var_tmp) *)
  (*   (fun () -> *)
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

  if not sign_package then begin
    match Sys_unix.is_file [%string "%{db_root}/%{db_name}.sig"] with
    | `Yes -> raise (UsageError "database signature found, but signing is disabled\n")
    | `No | `Unknown -> ()
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

  let makepkg_common_args =
    Option.value_map makepkg_conf ~default:[] ~f:(fun c -> [ "--config"; c ])
    @ Option.value_map buildscript ~default:[] ~f:(fun b -> [ "-p"; b ])
    @ args_from_boolean
        [
          (ignorearch, "--ignorearch");
          (noconfirm, "--noconfirm");
          (rmdeps, "--rmdeps");
          (syncdeps, "--syncdeps");
        ]
  in

  let get_pkglist db_root ?buildscript () = () in

  let startdir = Sys_unix.getcwd () in
  List.iter pkgdirs ~f:(fun pkg ->
      Sys_unix.chdir startdir;
      Sys_unix.chdir pkg;
      match Sys_unix.is_file "PKGBUILD" with
      | `Yes -> begin
          if run_pkgver then run_exn "makepkg" ("-od" :: makepkg_common_args);
          if not force then begin
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
            if List.length existing_pkglist = List.length pkglist then begin
              Printf.eprintf "%s: warning: skipping existing package (use -f to overwrite)\n"
                argv0;
              List.iter existing_pkglist ~f:(fun p -> Printf.eprintf "%S\n" p)
            end
          end
        end
      | `No | `Unknown -> raise (UsageError [%string "PKGBUILD does not exist for %{pkg}"]))

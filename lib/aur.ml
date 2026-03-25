open Lwt.Syntax
open Errors
open Utils

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Cohttp.Header.init_with "User-Agent" "oaur"

(* TODO: check how alad installs pacman deps *)
(* TODO: supress backtraces when printing error messages *)


let search term  =
  let search_aur term =
    let aururl = Uri.of_string aur_location in
    let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/search/" ^ term in
    let query = Uri.with_path aururl path in
    let open Lwt.Syntax in
    let open Cohttp_lwt_unix in
    let* (_,body) = Client.get ?headers:(Some ua_header) query in
    Cohttp_lwt.Body.to_string body
  in
  let display_search_results body =
    let osc8_link uri label =
      let st = "\o033\\" in
      let osc8 = "\o033]8" in
      Printf.sprintf "%s;;%s%s%s%s;;%s" osc8 uri st label osc8 st
    in
    let colored_pkg_name name =
      let fmt = Format.get_str_formatter () in
      Ocolor_format.prettify_formatter fmt;
      Format.fprintf fmt "@{<bold>@{<blue>aur@}/%s@}" name;
      Format.flush_str_formatter ()
    in
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    let results = from_string body |> member "results" |> to_list in
    List.iter (fun result ->
        let name = member "Name" result |> to_string in
        let ver = member "Version" result |> to_string in
        let numvotes = member "NumVotes" result |> to_int |> string_of_int in
        let popularity = member "Popularity" result |> to_number in
        let descr = member "Description" result |> to_string in
        let ood = match member "OutOfDate" result |> to_number_option with
          | None -> ""
          | Some epoch -> Core_unix.strftime (Core_unix.gmtime epoch) "(Out-of-date: %d %B %Y)"
        in
        let label = colored_pkg_name name in
        let pre = osc8_link (aur_location ^ "/packages/" ^ name) label in
        let rest_fmt : _ format =
          "%s @{<bold>@{<green>%s@}@} (+%s %.2f%%) @{<bold;red>%s@}\n    %s\n" in
        Ocolor_format.printf rest_fmt pre ver numvotes popularity ood descr
      )
      results;
    Lwt.return ()
  in
  let open Lwt.Syntax in
  let* body =  search_aur term in
  display_search_results body


let depends pkgname =
  let fetch_deps pkgname =
    let query_aur pkgname =
      let aururl = Uri.of_string aur_location in
      let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/info" in
      let url = Uri.with_path aururl path in
      let query = Uri.add_query_param url ("arg[]", [pkgname]) in
      let open Lwt.Syntax in
      let open Cohttp_lwt_unix in
      let* (_,body) = Client.get ?headers:(Some ua_header) query in
      Cohttp_lwt.Body.to_string body
      in
  let extract_results body =
    let open Yojson.Basic.Util in
    Yojson.Basic.from_string body
          |> member "results" |> to_list |> List.hd
    |> member "Depends" |> to_list |> List.map to_string
    in
  let* body = query_aur pkgname in
  Lwt.return(extract_results body) in

  let open Lwt.Syntax in
  let* deps = fetch_deps pkgname in
  List.iter (fun r -> Printf.printf "%s\n" r) deps;
  Lwt.return()




let fetch_exn syncmode discard pkgname  = 
  let pkglocation = aur_location ^ "/" ^ pkgname in
  let command = ("git", ["git";"ls-remote"; "--exit-code"; "-q"; pkglocation]) in
  let status = run_noexn command in
  ignore
    (match status with
     | Unix.WEXITED 0 -> ()
     | Unix.WEXITED _ -> raise (SubExn (Printf.sprintf "Pkg %s is not in AUR\n" pkgname))
     | _ -> raise (SubExn "Subprocess error"));

  let pathtocheck = Filename.concat pkgname ".git" in
  let pathclean = not (Sys.file_exists pathtocheck && Sys.is_directory pathtocheck) in

  (*TODO add results file? *)

  if pathclean then
    run ("git", ["git"; "clone"; pkglocation])

  else
    let git = ["git"; "-C"; pkgname] in 
    let (@) = List.append in
    let sync_should_merge upstream dest =
      let status =
        run_noexn("git", git @ ["merge-base"; "--is-ancestor"; upstream; dest] )
      in
      match status with
      | Unix.WEXITED 0 -> false
      | Unix.WEXITED 1 -> true
      | _ -> raise (SubExn "git merge-base error")

    in (* sync code goes here*)

    (*TODO write new wrapper of flock*)

    (* let fd = Unix.openfile pathtocheck [Unix.O_RDONLY] 0o640 in *)
    (* Flock.flock fd LOCK_EX; *)

    run("git", git @ ["fetch"; "origin"]);

    (* let orig_head = String.trim( *)
    (*     run_read_all("git", git @ ["rev-parse"; "--verify"; "HEAD"])) in *)

    let should_merge = sync_should_merge "origin/HEAD" "HEAD" in
    let upstream = "origin/HEAD" in
    if should_merge then
      let syncmode = Option.value ~default:("merge") syncmode in
      let dest = match syncmode with
        | "merge" | "rebase" -> "HEAD"
        | "reset" | "fetch" -> upstream
        | badsyncmode -> failwith (Printf.sprintf "Bad syncmode: %s" badsyncmode)
      in
      try
        match syncmode with
        | "merge"  ->
          if discard then
            run("git", git @ ["checkout"; "./"]);
          run("git", git @ ["merge"; upstream]);
        | "rebase" ->
          if discard then
            run("git", git @ ["checkout"; "./"]);
          run("git", git @ ["rebase"; upstream]);
        | "reset"  -> run("git", git @ ["reset"; "--hard"; dest])
        | "fetch"  -> ()
        | badsyncmode ->
          failwith (Printf.sprintf "Bad syncmode: %s" badsyncmode)
      with SubExn _ ->
        raise (SubExn (Printf.sprintf "fetch: failed to %s %s" syncmode pkgname))

    (* Flock.flock fd LOCK_UN; *)


(*spec: if --recurse passed use depends with a tsort algo *)
(*  to generate list, fetch should then handle lists and the *)
(*  argument passed by cli should be wrapped in said list *)
(*TODO cli should pass list*)
 
let fetch pkgnames syncmode discard =
  try
    List.iter (fetch_exn syncmode discard) pkgnames
  with
    SubExn msg -> Printf.printf "Error: %s" msg



let chroot
      build update create path
      directory
      bind_ro bind_rw
      pkgnames
      makechrootpkg_args
      makechrootpkg_makepkg_args
  =
  (*TODO eventually change interface to --action, mutually exclusive in definition*)
  let not_only_one a b c d =
    let count = List.fold_left (fun acc x -> if x then acc + 1 else acc) 0 [a; b; c; d] in
    count <> 1
  in

  if not_only_one build update create path then
    raise (UsageError  "More than one of update create and build was selected");
  
  let (//) = Filename.concat in
  let directory_exists d = Sys.file_exists d && Sys.is_directory d in

  let rec default_first lst =
    match lst with
    | hd::tl  -> if Sys.file_exists hd then hd else default_first tl
    | [] -> raise (Failure
                     (Printf.sprintf "A required file is missing in the list: %s"
                        (String.concat " " lst)))
  in

  let machine = String.trim (run_read_all ("uname", ["uname"; "-m"])) in

  let etcdir = "/etc/aurutils" in
  let shrdir = "/usr/share/devtools" in
  let directory = Option.value ~default:("/var/lib/aurbuild" // machine) directory in
  let default_pacman_paths = [
    etcdir // "pacman-" ^ machine ^ ".conf";
    shrdir // "pacman.conf.d" // "aurutils" ^ machine ^ ".conf"
  ] in
  let
 default_makepkg_paths = [
    etcdir // "makepkg-" ^ machine ^ ".conf";
    shrdir // "makepkg.conf.d" // machine ^ ".conf"
  ] in

  let pacman_conf = default_first default_pacman_paths in
  let makepkg_conf = default_first default_makepkg_paths in

  (* let aur_pacman_auth = [|"sudo"; "--preserve-env=GNUGPGHOME,SSH_AUTH_SOCK,PKGDEST"|] in *)
  if create then
    let base_packages =
      if pkgnames <> [] then
        pkgnames
      else
        let multilib_in_conf = run_read_all
            ("pacini",
             ["pacini"; "--section=multilib"; pacman_conf]) in
        if multilib_in_conf <> "" && machine = "x86_64" then
          ["base-devel"; "multilib-devel"]
        else
          ["base-devel"]
    in
    if not (directory_exists directory)  then
      run ("sudo", ["sudo"; "install"; "-d"; directory; "-m"; "755"; "-v" ]);

    if not (directory_exists (directory // "root")) then
      run ~suppress_output:false
        ("sudo",
         ["sudo";
          "mkarchroot";
          "-C"; pacman_conf;
          "-M"; makepkg_conf;
          directory // "root";
          (String.concat " " base_packages)
         ]);

  else
    if not (directory_exists (directory // "root")) then
      raise (Failure (Format.sprintf "chroot: %S is not a directory\n
                                              chroot: did you run aur chroot --create\n"
                                      (directory // "root")));

    let r = Str.regexp {|Server = file://\(.*\)|} in
    let inp = Unix.open_process_args_in "pacman-conf" [|"pacman-conf"; "--config"; pacman_conf|] in
    let rec find_dirs_to_bindmount_rw ic lines =
      let line = In_channel.input_line ic in
      match line with
      | Some l -> let result = Str.string_match r l 0 in
        if result then
          find_dirs_to_bindmount_rw ic ((Str.matched_group 1 l)::lines)
        else
          find_dirs_to_bindmount_rw ic lines
      | None -> List.rev lines
    in
    let bindmounts_rw_from_conf= find_dirs_to_bindmount_rw inp [] in

    if update then
      let bind_ro = String.concat " "
          (List.map (fun b -> "--bind-ro=" ^ b ) bind_ro)
      in
      let bind_rw = String.concat " "
          (List.map
             (fun b_rw -> "--bind=" ^ b_rw)
             (List.append bind_rw bindmounts_rw_from_conf))
      in
      run ("sudo",
           ["sudo";
            "arch-nspawn";
            "-C"; pacman_conf;
            "-M"; makepkg_conf;
            directory // "root";
            bind_rw;
            bind_ro;
            "pacman"; "-Syu"; "--noconfirm";
            (String.concat " " pkgnames)
           ])

    else
    if build then
      let bind_ro = String.concat " "
          (List.map (fun b_ro -> "-D" ^ b_ro) bind_ro) in
      let bind_rw = String.concat " "
          (List.map (fun b_rw -> "-d" ^ b_rw) bindmounts_rw_from_conf) in
      run ("sudo",
           ["sudo"; "--preserve-env=GNUPGHOME,SSH_AUTH_SOCK,PKGDEST";
            "makechrootpkg";
            "-r"; directory;
            bind_ro; bind_rw;
            String.concat " " makechrootpkg_args;
            "--";
            String.concat " " makechrootpkg_makepkg_args;
           ])
    else
      (* if path *)
      let realpath = String.trim(run_read_all("realpath",
                                              ["realpath"; "--" ; directory // "root"])) in

      print_endline realpath;

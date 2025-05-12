open Lwt.Syntax

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Cohttp.Header.init_with "User-Agent" "oaur"

(* TODO: check how alad installs pacman deps *)
(* TODO: supress backtraces when printing error messages *)

exception SubExn of string
exception UsageError of string


let run ?(suppress_output=true) (command, args) =
  let args_array = Array.of_list(List.filter (fun str -> str <> "" ) args) in
  if suppress_output then
    let dev_null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
    let pid = Unix.create_process command args_array Unix.stdin dev_null dev_null in
    let (_,status) = Unix.waitpid [] pid in
    Unix.close dev_null;
    match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code -> raise (SubExn (Printf.sprintf "Subprocess exited with code %d" code))
    | _ -> raise (SubExn "Subprocess error")
  else
  let (_,status) =
    Unix.waitpid []
      (Unix.create_process command args_array Unix.stdin Unix.stderr Unix.stderr) in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> raise (SubExn (Printf.sprintf "Subprocess exited with code %d" code))
  | _ -> raise (SubExn "Subprocess error")

let run_noexn ?(suppress_output=true) (command,args) =
  let args_array = Array.of_list(List.filter (fun str -> str <> "" ) args) in
  if suppress_output then
    let dev_null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
    let pid = Unix.create_process command args_array Unix.stdin dev_null dev_null in
    let (_,status) = Unix.waitpid [] pid in
    Unix.close dev_null;
    status
  else
    let (_, status) =
      Unix.waitpid []
        (Unix.create_process command args_array Unix.stdin Unix.stderr Unix.stderr) in
    status


let run_read_all (cmd,args) =
  let args_array = Array.of_list(List.filter (fun str -> str <> "") args) in
  let inp = Unix.open_process_args_in cmd args_array in
  let r = In_channel.input_all inp in
  In_channel.close inp; r


let search term  =
  let build_query term  =
    let aururl = Uri.of_string aur_location in
    let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/search/" ^ term in
    Uri.with_path aururl path
  in
  let search_aur query =
    let open Lwt.Syntax in
    let open Cohttp_lwt_unix in
    let*(_,body) = Client.get ?headers:(Some ua_header) query in
    Cohttp_lwt.Body.to_string body
  in
  let display_search_results body =
    let st = "\o033\\" in
    let osc8 = "\o033]8" in

    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    let results =  from_string body |> member "results" |> to_list in
    List.iter (fun json ->
        let name = member "Name" json |> to_string in
        let ver = member "Version" json |> to_string in
        let numvotes = member "NumVotes" json |> to_int |> string_of_int in
        let popularity = member "Popularity" json |> to_number in
        let descr = member "Description" json |> to_string in
        let ood = match (member "OutOfDate" json |> to_number_option) with
          | None -> ""
          | Some epoch -> Core_unix.strftime (Core_unix.gmtime epoch) "(Out-of-date: %d %B %Y)" in

        (*add osc8 style link*)
        let fmt = Format.get_str_formatter () in Ocolor_format.prettify_formatter fmt;
        Format.fprintf fmt "@{<bold>@{<blue>aur@}/%s@}" name;
        let pre = Format.flush_str_formatter () in
        (* (\* # OSC8;;URI ST <name> OSC8;;ST ) *\) *)
        let pre = Format.asprintf "%s;;%s%s%s%s;;%s"
                    osc8 (aur_location ^ "/packages/" ^ name) st pre osc8 st in

        let rest_fmt : _ format =
          "%s @{<bold>@{<green>%s@}@} (+%s %.2f%%) @{<bold;red>%s@}\n    %s\n" in
        Ocolor_format.printf rest_fmt pre ver numvotes popularity ood descr
      )
      results;
    Lwt.return()
  in
  let open Lwt.Syntax in
  let* body = build_query term |> search_aur in
  display_search_results body

let fetch_deps pkgname =
  let build_query pkgname =
    let aururl = Uri.of_string aur_location in
    let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/info" in
    let url = Uri.with_path aururl path in
    Uri.add_query_param url ("arg[]", [pkgname])
  in
  let query_aur query =
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
  let query = build_query pkgname in
  let* body = query_aur query in
  Lwt.return(extract_results body)


let check_if_aur pkgname =
  let build_query pkgname =
    let aururl = Uri.of_string aur_location in
    let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/info" in
    let url = Uri.with_path aururl path in
    Uri.add_query_param url ("arg[]", [pkgname])
  in
  let check_rpc query =
    let* (_,body) = Cohttp_lwt_unix.Client.get
                      ?headers:(Some ua_header) query in
    Cohttp_lwt.Body.to_string body
  in
  let extract_result body =
    let open Yojson.Basic.Util in
    let resultcount = Yojson.Basic.from_string body
                      |> member "resultcount" |> to_int
    in
    if resultcount > 1 then true else false
  in
  let query = build_query pkgname in
  let* body = check_rpc query in
  Lwt.return(extract_result body)


let classify_deps results =
  let rec classify_deps_aux pkgnames repo aur other =
    match pkgnames with
    | pkgname::tl ->
      let pacman_status = run_noexn("pacman",["pacman"; "-Sqi"; pkgname]) in
      (match pacman_status with
       | Unix.WEXITED 0 -> classify_deps_aux tl (pkgname::repo) aur other
       | Unix.WEXITED _ ->
         let* isaur = check_if_aur pkgname in
         if isaur then
           classify_deps_aux tl repo (pkgname::aur) other
         else
           classify_deps_aux tl repo aur (pkgname::other)
       | _ -> classify_deps_aux tl repo aur (pkgname::other))
    | [] -> Lwt.return(repo,aur,other)
  in
  classify_deps_aux results [] [] []


(* TODO design datastructure for recursively classifying deps
   and pretty printer for it *)
let depends pkgname =
  let open Lwt.Syntax in
  let* deps = fetch_deps pkgname in
  let* (repo,aur,other) = classify_deps deps in
  Printf.printf "repo pkgs: \n";
  List.iter (fun r -> Printf.printf "  %s\n" r) repo;
  Printf.printf "aur pkgs: \n";
  List.iter (fun r -> Printf.printf "  %s\n" r) aur;
  Printf.printf "other pkgs: \n";
  List.iter (fun r -> Printf.printf "  %s\n" r) other;
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

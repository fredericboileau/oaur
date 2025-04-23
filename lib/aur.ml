(*TODO simple subprocess*)

(* open Lwt *)
open Cohttp
(* open Cohttp_lwt_unix *)
(* open Lwt.Syntax *)

(* TODO: check how alad installs pacman deps *)

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Header.init_with "User-Agent" "oaur"


exception SubExn of string
(*TODO how to redirect stdout to stderr*)
let run command =
  let open Lwt.Syntax in
  let* p =
    Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null command
  in
  match p with
  | Unix.WEXITED 0 -> Lwt.return()
  | _ -> raise (SubExn "Subprocess error")


let run3 (command, args) =
  let (_,status) = Unix.waitpid [] (Unix.create_process
          command args Unix.stdin Unix.stderr Unix.stderr) in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> raise (SubExn (Printf.sprintf "Subprocess exited with code %d" code))
  | _ -> raise (SubExn "Subprocess error")

let run2 (command,args) =
  print_endline command;
  print_endline (String.concat " "  (Array.to_list args));
  run3(command,args)

let run_noexn (command,args) =
  let (_, status) =
    Unix.waitpid []
      (Unix.create_process command args Unix.stdin Unix.stderr Unix.stderr) in
  status

let run_read_all (cmd,args) =
  let inp = Unix.open_process_args_in cmd args in
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
      results
  in
  let open Lwt in
  build_query term |> search_aur >|= fun body -> display_search_results body

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
    let*(_,body) = Client.get ?headers:(Some ua_header) query in
    Cohttp_lwt.Body.to_string body
  in
  let extract_results body =
    let open Yojson.Basic.Util in
    Yojson.Basic.from_string body
    |> member "results" |> to_list |> List.hd
    |> member "Depends" |> to_list |> List.map to_string
  in
  let open Lwt in
  build_query pkgname |> query_aur >|= fun body -> extract_results body



(* TODO wrap this in tree like structure *)
type check_if_repo = { pkgname: string; status: Unix.process_status }
type classify_deps = { repo: string list; aur: string list; other: check_if_repo list }


let check_if_repo pkgname =
  let open Lwt.Syntax in
  let command pkgname = ("", [|"pacman";"-Sqi"; pkgname|]) in
  let* p  =  Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null (command pkgname) 
  in Lwt.return({ pkgname; status = p })


(*TODO simplify monads*)
let check_if_aur pkgname =
  let build_query pkgname =
    let aururl = Uri.of_string aur_location in
    let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/info" in
    let url = Uri.with_path aururl path in
    Uri.add_query_param url ("arg[]", [pkgname])
  in
  let check_rpc query =
    let open Lwt.Syntax in
    let open Cohttp_lwt_unix in
    let*(_,body) = Client.get ?headers:(Some ua_header) query in
    Cohttp_lwt.Body.to_string body
  in
  let extract_result body =
    let open Yojson.Basic.Util in
    let resultcount = Yojson.Basic.from_string body
                      |> member "resultcount" |> to_int
    in
    if resultcount > 1 then true else false
  in
  let open Lwt in
  build_query pkgname |> check_rpc >|= fun body -> extract_result body


let classify_deps results =
  let open Lwt in
  let rec classify_deps_aux results repo aur other =
    match results with
    | hd::tl ->
      (match hd.status with
       | Unix.WEXITED 0 -> classify_deps_aux tl (hd.pkgname::repo) aur other
       | Unix.WEXITED _ -> check_if_aur hd.pkgname
         >>= fun isaur ->
         if isaur then
           classify_deps_aux tl repo (hd.pkgname::aur) other
         else
           classify_deps_aux tl repo aur (hd::other)
       | _ -> classify_deps_aux tl repo aur (hd::other))
    | [] -> Lwt.return({repo;aur;other})
  in
  classify_deps_aux results [] [] []


(* TODO design datastructure for recursively classifying deps
   and pretty printer for it *)
let depends pkgname =
  let open Lwt.Syntax in
  let* deps = fetch_deps pkgname in
  let* results = Lwt.all (List.map check_if_repo deps) in
  let* classified = classify_deps results in
  Printf.printf "repo pkgs: \n";
  List.iter (fun r -> Printf.printf "  %s\n" r) classified.repo;
  Printf.printf "aur pkgs: \n";
  List.iter (fun r -> Printf.printf "  %s\n" r) classified.aur;
  Printf.printf "other pkgs: \n";
  List.iter (fun r -> Printf.printf "  %s\n" r.pkgname) classified.other;
  Lwt.return()




let fetch_exn syncmode discard pkgname  = 
  let pkglocation = aur_location ^ "/" ^ pkgname in
  let command = ("git", [|"git";"ls-remote"; "--exit-code"; pkglocation|]) in
  let status = run_noexn command in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED _ -> raise (SubExn (Printf.sprintf "Pkg %s is not in AUR\n" pkgname))
  | _ -> raise (SubExn "Subprocess error");

  let pathtocheck = Filename.concat pkgname ".git" in
  let pathclean = not (Sys.file_exists pathtocheck && Sys.is_directory pathtocheck) in

  (*TODO add results file? *)

  if pathclean then
    run3 ("git", [|"git"; "clone"; pkglocation|])

  else
    let git = [|"git"; "-C"; pkgname|] in 
    let (@@) = Array.append in
    let sync_should_merge upstream dest =
      let status =
        run_noexn("git", git @@ [|"merge-base"; "--is-ancestor"; upstream; dest|] )
      in
      match status with
      | Unix.WEXITED 0 -> false
      | Unix.WEXITED 1 -> true
      | _ -> raise (SubExn "git merge-base error")

    in (* sync code goes here*)

    Printf.printf "git directory for %s exists already, acquiring lock" pkgname;
    (*TODO check for --timeout*)
    (* let fd = Unix.openfile pathtocheck [Unix.O_RDONLY] 0o640 in *)
    (* Flock.flock fd LOCK_EX; *)

    run3("git", git @@ [|"fetch"; "origin"|]);

    let orig_head = String.trim(
                         run_read_all("git", git @@ [|"rev-parse"; "--verify"; "HEAD"|])) in
    Printf.printf "%s\n" orig_head;

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
               run3("git", git @@ [|"checkout"; "./"|]);
            run3("git", git @@ [|"merge"; upstream|]);
          | "rebase" ->
              if discard then
               run3("git", git @@ [|"checkout"; "./"|]);
            run3("git", git @@ [|"rebase"; upstream|]);
          | "reset"  -> run3("git", git @@ [|"reset"; "--hard"; dest|])
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




exception UsageError of string

let chroot
      build update create path
      bind_ro bind_rw
      pkgnames
      makechrootpkg_args
      makechrootpkg_makepkg_args
  =
  (*TODO eventually change interface to --action, mutually exclusive in definition*)
  let more_than_one_true a b c =
    (a && b) || (a && c) || (b && c) in
  if more_than_one_true build update create then
    raise (Failure  "More than one of update create and build was selected");
  
  let (//) = Filename.concat in
  let directory_exists d = Sys.file_exists d && Sys.is_directory d in

  let rec default_first lst =
    match lst with
    | hd::tl  -> if Sys.file_exists hd then hd else default_first tl
    | [] -> raise (Failure
                     (Printf.sprintf "A required file is missing in the list: %s"
                        (String.concat " " lst)))
  in

  let machine = String.trim (run_read_all ("uname", [|"uname"; "-m"|])) in

  let etcdir = "/etc/aurutils" in
  let shrdir = "/usr/share/devtools" in
  let directory = "/var/lib/aurbuild" // machine in
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
    begin
      let base_packages =
        if pkgnames <> [] then
          pkgnames
        else
          let multilib_in_conf = run_read_all
                                   ("pacini",
                                    [|"pacini"; "--section=multilib"; pacman_conf|]) in
          if multilib_in_conf <> "" && machine = "x86_64" then
            ["base-devel"; "multilib-devel"]
          else
            ["base-devel"]
      in
      if not (directory_exists directory)  then
        run3 ("sudo", [|"sudo"; "install"; "-d"; directory; "-m"; "755"; "-v" |]);

      if not (directory_exists (directory // "root")) then
        run3 ("sudo",
              [|"sudo";
                "mkarchroot";
                "-C"; pacman_conf;
                "-M"; makepkg_conf;
                directory // "root";
                (String.concat " " base_packages)
              |]);


      if not (directory_exists (directory // "root")) then
        raise (Failure (Format.sprintf "chroot: %S is not a directory\n
                                        chroot: did you run aur choor -create\n"
                          (directory // "root")));

    end
  else
    let r = Str.regexp {|Server = file://\(.*\)|} in
    let inp = Unix.open_process_args_in
                "pacman-conf" [|"pacman-conf"; "--config"; pacman_conf|] in
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
    let bindmounts_from_conf_rw = find_dirs_to_bindmount_rw inp [] in

    if update then
      begin


        let bind_ro = String.concat " "
                        (List.map (fun b -> "--bind-ro=" ^ b ) bind_ro)
        in
        let bind_rw = String.concat " "
                        (List.map
                           (fun b_rw -> "--bind=" ^ b_rw)
                           (List.append bind_rw bindmounts_from_conf_rw))
        in
        let args =
          List.filter (fun str -> str <> "")
            ["sudo";
             "arch-nspawn";
             "-C"; pacman_conf;
             "-M"; makepkg_conf;
             directory // "root";
             bind_rw;
             bind_ro;
             "pacman"; "-Syu"; "--noconfirm";
             (String.concat " " pkgnames)
            ]
        in
        run3 ("sudo", Array.of_list args)

      end
    else
      (* if build *)
      let bind_ro = String.concat " "
                      (List.map (fun b_ro -> "-D" ^ b_ro) bind_ro) in
      let bind_rw = String.concat " "
                      (List.map (fun b_rw -> "-d" ^ b_rw) bindmounts_from_conf_rw) in
      let args =
        List.filter (fun str -> str <> "")
          ["sudo"; "--preserve-env=GNUPGHOME,SSH_AUTH_SOCK,PKGDEST";
           "makechrootpkg";
           "-r"; directory;
           bind_ro; bind_rw;
           String.concat " " makechrootpkg_args;
           "--";
           String.concat " " makechrootpkg_makepkg_args;
          ]
      in
      run3("sudo", Array.of_list args)

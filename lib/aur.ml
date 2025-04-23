(*TODO simple subprocess*)

(* open Lwt *)
open Cohttp
(* open Cohttp_lwt_unix *)
(* open Lwt.Syntax *)

(* TODO: check how alad installs pacman deps *)

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Header.init_with "User-Agent" "oaur"

(*pretty printing*)

let pp_string_list fmt lst =
  let open Format in
  fprintf fmt "[%a]"
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_print_string)
    lst

let pp_string_list_raw fmt lst =
  let open Format in
  fprintf fmt "%a"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)

    lst

let pp_array printer fmt arr =
  Array.iteri (fun i x ->
    if i > 0 then Format.fprintf fmt " ";
    Format.fprintf fmt "%a" printer x
  ) arr

let array_to_string arr =
  Format.asprintf "%a" (pp_array Format.pp_print_string) arr

let string_of_list lst =
  String.concat " " lst


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

let run2 command =
  match  (Unix.system (array_to_string command)) with
  | Unix.WEXITED 0 -> ()
  | _ -> raise (SubExn "Subprocess error")

let run3 (command, args) =
  let (_,status) = Unix.waitpid [] (Unix.create_process
          command args Unix.stdin Unix.stderr Unix.stderr) in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> raise (SubExn (Printf.sprintf "Subprocess exited with code %d" code))
  | _ -> raise (SubExn "Subprocess error")

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
        let results_frmt: _ format =
          "@{<bold>@{<blue>aur@}/%s @{<green>%s@}@} (+%s %.2f%%) @{<bold;red>%s@}\n    %s\n"  in
        Ocolor_format.printf results_frmt name ver numvotes popularity ood descr
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




let fetch_exn pkgname syncmode discard = 
  let open Lwt.Syntax in
  let pkglocation = aur_location ^ "/" ^ pkgname in
  let command = ("git", [|"git";"ls-remote"; "--exit-code"; pkglocation|]) in
  let* p = Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null command in
  let* () = match p with
    | Unix.WEXITED 0 -> Lwt.return()
    | Unix.WEXITED _ -> raise (SubExn (Printf.sprintf "Pkg %s is not in AUR\n" pkgname))
    | _ -> raise (SubExn "Subprocess error")
  in

  let pathtocheck = Filename.concat pkgname ".git" in
  let pathclean = not (Sys.file_exists pathtocheck && Sys.is_directory pathtocheck) in
  (*TODO add results file? *)

  if pathclean then

    let* () = run ("git", [|"git"; "clone"; pkglocation|])
    in Lwt.return()

  else
    let git = [|"git"; "-C"; pkgname|] in 
    let (@@) = Array.append in
    let sync_should_merge upstream dest =
      let* p =
        Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null
          ("git", git @@ [|"merge-base"; "--is-ancestor"; upstream; dest|] )
      in
      match p with
      | Unix.WEXITED 0 -> Lwt.return(false)
      | Unix.WEXITED 1 -> Lwt.return(true)
      | _ -> raise (SubExn "git merge-base error")

    in (* sync code goes here*)

    let* () = Lwt_io.printlf "git directory for %s exists already, acquiring lock" pkgname in
    (*TODO check for --timeout*)
    (* let fd = Unix.openfile pathtocheck [Unix.O_RDONLY] 0o640 in *)
    (* Flock.flock fd LOCK_EX; *)

    let*() = run ("git", git @@ [|"fetch"; "origin"|]) in

    let* orig_head = Lwt_process.pread_line ("git", git @@ [|"rev-parse"; "--verify"; "HEAD"|]) in
    Printf.printf "%s\n" orig_head;

    let* should_merge = sync_should_merge "origin/HEAD" "HEAD" in
    let* () =
      let upstream = "origin/HEAD" in
      if should_merge then
        let syncmode = Option.value ~default:("merge") syncmode in
        let dest = match syncmode with
          | "merge" | "rebase" -> "HEAD"
          | "reset" | "fetch" -> upstream
          | badsyncmode -> failwith (Printf.sprintf "Bad syncmode: %s" badsyncmode)
        in
        try%lwt
          match syncmode with
          | "merge"  ->
            let* () =
              if discard then
               run("git", git @@ [|"checkout"; "./"|])
            else
              Lwt.return()
            in
            run("git", git @@ [|"merge"; upstream|])
          | "rebase" ->
            let%lwt () =
              if discard then
               run("git", git @@ [|"checkout"; "./"|])
            else
              Lwt.return()
            in
            run ("git", git @@ [|"rebase"; upstream|])

          | "reset"  -> run ("git", git @@ [|"reset"; "--hard"; dest|])
          | "fetch"  -> Lwt.return()
          | badsyncmode ->
            failwith (Printf.sprintf "Bad syncmode: %s" badsyncmode)

        with SubExn _ ->
          raise (SubExn (Printf.sprintf "fetch: failed to %s %s" syncmode pkgname))
      else
        Lwt.return()
    in

    (* Flock.flock fd LOCK_UN; *)
    Lwt.return()


(*spec: if --recurse passed use depends with a tsort algo *)
(*  to generate list, fetch should then handle lists and the *)
(*  argument passed by cli should be wrapped in said list *)
(*TODO cli should pass list*)
 
let fetch pkgnames syncmode discard =
  try%lwt fetch_exn (List.hd pkgnames) syncmode discard
  with SubExn msg -> Printf.printf "Error: %s" msg; Lwt.return()




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

  let string_of_string_list lst = "[" ^ String.concat "; " lst ^ "]" in
  let rec default_first lst =
    match lst with
    | hd::tl  -> if Sys.file_exists hd then hd else default_first tl
    | [] -> raise (Failure (Printf.sprintf "A required file is missing in the list: %s"
                             (string_of_string_list lst)))
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
                (Format.asprintf "%a" pp_string_list_raw base_packages)|]);


      if not (directory_exists (directory // "root")) then
        raise (Failure (Format.sprintf "chroot: %S is not a directory\n
                                        chroot: did you run aur choor -create\n"
                          (directory // "root")));

    end
  else
    if update then
      begin

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
          let bind_ro = String.concat " "
                          (List.map (fun b -> "--bind-ro=" ^ b ) bind_ro)
          in
          let bind_rw = String.concat " "
                          (List.map
                             (fun b_rw -> "--bind-rw=" ^ b_rw)
                             (List.append bind_rw bindmounts_from_conf_rw))
          in
          run3 ("sudo",
                [|"sudo";
                  "arch-nspawn";
                  "-C"; pacman_conf;
                  "-M"; makepkg_conf;
                  directory // "root";
                  bind_rw; bind_ro;
                  "pacman"; "-Syu"; "--noconfirm";
                  String.concat " " pkgnames;
                |])

      end
    else
      (* if build *)
      let bind_ro = String.concat " "
                      (List.map (fun b_ro -> "-D" ^ b_ro) bind_ro) in
      let bind_rw = String.concat " "
                      (List.map (fun b_rw -> "-d" ^ b_rw) bind_rw) in
      print_endline (String.concat " " makechrootpkg_args);
      print_endline (String.concat " " makechrootpkg_makepkg_args);
      run3("sudo",
           [|"sudo"; "--preserve-env=GNUPGHOME,SSH_AUTH_SOCK,PKGDEST";
             "makechrootpkg";
             "-r"; directory;
             bind_ro; bind_rw;
             String.concat " " makechrootpkg_args;
             "--";
             String.concat " " makechrootpkg_makepkg_args;
           |])

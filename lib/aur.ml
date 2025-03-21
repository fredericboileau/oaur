open Lwt
open Cohttp
open Cohttp_lwt_unix

(* TODO: check how alad installs pacman deps *)

let aur_location = "https://aur.archlinux.org"
let aur_rpc_ver = 5
let ua_header = Header.init_with "User-Agent" "oaur"


let search term  =
    let build_query term  =
        let aururl = Uri.of_string aur_location in
        let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/search/" ^ term in
        Uri.with_path aururl path
    in
    let search_aur query =
        Client.get ?headers:(Some ua_header) query
        >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
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
    build_query term |> search_aur >|= fun body -> display_search_results body

let fetch_deps pkgname =
    let build_query pkgname =
        let aururl = Uri.of_string aur_location in
        let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/info" in
        let url = Uri.with_path aururl path in
        Uri.add_query_param url ("arg[]", [pkgname])
    in
    let query_aur query =
        Client.get ?headers:(Some ua_header) query
        >>= fun (_,body) -> Cohttp_lwt.Body.to_string body
    in
    let extract_results body =
        let open Yojson.Basic.Util in
        Yojson.Basic.from_string body
           |> member "results" |> to_list |> List.hd
           |> member "Depends" |> to_list |> List.map to_string
    in
    build_query pkgname |> query_aur >|= fun body -> extract_results body



(* TODO wrap this in tree like structure *)
type check_if_repo = { pkgname: string; status: Unix.process_status }
type classify_deps = { repo: string list; aur: string list; other: check_if_repo list }


let check_if_repo pkgname =
    let command pkgname = ("pacman", [|"pacman";"-Sqi"; pkgname|]) in
    Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null (command pkgname) 
    >|= fun p -> { pkgname; status = p }


let check_if_aur pkgname =
    let build_query pkgname =
        let aururl = Uri.of_string aur_location in
        let path = "/rpc" ^ "/v" ^ (string_of_int aur_rpc_ver) ^ "/info" in
        let url = Uri.with_path aururl path in
        Uri.add_query_param url ("arg[]", [pkgname])
    in
    let check_rpc query =
        Client.get ?headers:(Some ua_header) query
        >>= fun(_,body) -> Cohttp_lwt.Body.to_string body
    in
    let extract_result body =
        let open Yojson.Basic.Util in
        let resultcount = Yojson.Basic.from_string body
           |> member "resultcount" |> to_int
        in
        if resultcount > 1 then true else false
    in
    build_query pkgname |> check_rpc >|= fun body -> extract_result body


let classify_deps results =
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
    let%lwt deps = fetch_deps pkgname in
    let%lwt results = Lwt.all (List.map check_if_repo deps) in
    let%lwt classified = classify_deps results in
    Printf.printf "repo pkgs: \n";
    List.iter (fun r -> Printf.printf "  %s\n" r) classified.repo;
    Printf.printf "aur pkgs: \n";
    List.iter (fun r -> Printf.printf "  %s\n" r) classified.aur;
    Printf.printf "other pkgs: \n";
    List.iter (fun r -> Printf.printf "  %s\n" r.pkgname) classified.other;
    Lwt.return()


exception SubExn of string
let fetch_exn pkgname = 
    let pkglocation = aur_location ^ "/" ^ pkgname in
    let command = ("git", [|"git";"ls-remote"; "--exit-code"; pkgname|]) in
    let%lwt p = Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null command in
    let%lwt () = match p with
        | Unix.WEXITED 0 -> Lwt.return()
        | Unix.WEXITED _ -> Lwt.fail (SubExn (Printf.sprintf "Pkg %s is not in AUR\n" pkgname))
        | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> raise (Failure "Subprocess error")
    in
    let%lwt p = 
        let command = ("git", [|"git"; "clone"; pkglocation|]) in
        Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null command
    in
    let%lwt () =  match p with
        | Unix.WEXITED 0 -> Lwt.return()
        | _ -> Lwt.fail_with (Printf.sprintf "Could not clone %s" pkgname)
    in
    Lwt.return()


let fetch pkgname = 
    try%lwt fetch_exn pkgname 
    with SubExn msg -> Printf.printf "Error: %s" msg; Lwt.return()

(* let exec_exn ~stdout ~stderr command =  *)
(*     Lwt_process.exec ~stdout:stdout ~stderr:stderr command *)
(*     >>= fun p -> match p with *)
(*     | Unix.WEXITED 0 -> Lwt.return() *)
(*     | Unix.WEXITED _ -> raise (Failure "Pkg does not exist") *)
(*     | Unix.WSIGNALED _ | Unix.WSTOPPED  _ ->  *)
(*             raise (Failure "Process got signaled/stopped") *)
(**)
(* let fetch pkgname =  *)
    (* let command = ( *)
    (*     "git",  *)
    (*     [|"git";"ls-remote"; "--exit-code"; aur_location ^ "/" ^ pkgname|] *)
    (* ) in *)
(*     exec_exn ~stdout:`Dev_null ~stderr:`Dev_null command; *)
(*     let command = ( *)
(*         "git", *)
(*         [|"git"; "clone" *)
(*     exec_exn ~stdout:`Dev_null ~stderr:`Dev_null *)




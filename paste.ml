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
    try fetch_exn pkgname 
    with SubExn msg -> Printf.printf "Error: %s" msg; Lwt.return()

let fetch pkgname = 
    let command = (
        "git", 
        [|"git";"ls-remote"; "--exit-code"; aur_location ^ "/" ^ pkgname|]
    ) in
    Lwt_process.exec ~stdout:`Dev_null ~stderr:`Dev_null command
    >>= (
        fun p -> match p with
        | Unix.WEXITED 0 -> Lwt.return()
        | Unix.WEXITED _ -> raise (Failure (Printf.sprintf"Pkg %s is not in AUR\n" pkgname))
        | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> raise (Failure "Subprocess error")
        )
    >>= fun () -> Lwt_io.printf "Can clone"

exception SubExn of string

let run ?(suppress_output = true) (command, args) =
  let args_array = Array.of_list (List.filter (fun str -> str <> "") args) in
  if suppress_output then (
    let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o666 in
    let pid =
      Unix.create_process command args_array Unix.stdin dev_null dev_null
    in
    let _, status = Unix.waitpid [] pid in
    Unix.close dev_null;
    match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code ->
        raise (SubExn (Printf.sprintf "Subprocess exited with code %d" code))
    | _ -> raise (SubExn "Subprocess error"))
  else
    let _, status =
      Unix.waitpid []
        (Unix.create_process command args_array Unix.stdin Unix.stderr
           Unix.stderr)
    in
    match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED code ->
        raise (SubExn (Printf.sprintf "Subprocess exited with code %d" code))
    | _ -> raise (SubExn "Subprocess error")

let run_noexn ?(suppress_output = true) (command, args) =
  let args_array = Array.of_list (List.filter (fun str -> str <> "") args) in
  if suppress_output then (
    let dev_null = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o666 in
    let pid =
      Unix.create_process command args_array Unix.stdin dev_null dev_null
    in
    let _, status = Unix.waitpid [] pid in
    Unix.close dev_null;
    status)
  else
    let _, status =
      Unix.waitpid []
        (Unix.create_process command args_array Unix.stdin Unix.stderr
           Unix.stderr)
    in
    status

let run_read_all (cmd, args) =
  let args_array = Array.of_list (List.filter (fun str -> str <> "") args) in
  let inp = Unix.open_process_args_in cmd args_array in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

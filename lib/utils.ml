exception SubExn of int

let run_exn ?(to_stderr = false) ?(env = Unix.environment ()) cmd args =
  let args_array = Array.of_list (cmd :: args) in
  let stdout = if to_stderr then Unix.stderr else Unix.stdout in
  let pid =
    Unix.create_process_env cmd args_array env Unix.stdin stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code -> raise (SubExn code)
  | _ -> raise (SubExn 1)

let run ?(to_stderr = false) ?(env = Unix.environment ()) cmd args =
  let args_array = Array.of_list (cmd :: args) in
  let stdout = if to_stderr then Unix.stderr else Unix.stdout in
  let pid =
    Unix.create_process_env cmd args_array env Unix.stdin stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  status

let run_with_arg0 ?(to_stderr = false) ?(env = Unix.environment ()) name cmd =
  let cmd = Array.of_list cmd in
  let stdout = if to_stderr then Unix.stderr else Unix.stdout in
  let pid =
    Unix.create_process_env name cmd env Unix.stdin stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  status

let run_capture cmd args =
  let args_array = Array.of_list (cmd :: args) in
  let inp = Unix.open_process_args_in cmd args_array in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  String.trim r

let is_readable p =
  match Unix.access p [ Unix.R_OK ] with
  | () -> true
  | exception Unix.Unix_error _ -> false

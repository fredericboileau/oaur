open Aur.Utils

let oaur = "oaur"

let run ?(env = Unix.environment ()) cmd args =
  run_noexn ~suppress_output:false ~env (cmd, cmd :: args)
  |> exit_code_of_status

let run_capture cmd args = String.trim (run_read_all (cmd, cmd :: args))
let sudo args = run "sudo" args

let tmpdir = Filename.temp_dir "oaur-chroot-test" ""
let chroot_dir = Filename.concat tmpdir "aurbuild"
let pkg_dir = Filename.concat tmpdir "pkg"
let pkgs_dir = Filename.concat tmpdir "pkgs"

let () =
  at_exit (fun () ->
    ignore (sudo [ "umount"; "-R"; Filename.concat chroot_dir "root" ]);
    ignore (sudo [ "rm"; "-rf"; tmpdir ]))

let%test "rejects missing action" = run oaur [ "chroot" ] <> 0

let%test "rejects multiple actions" =
  run oaur [ "chroot"; "--create"; "--path"; "-D"; chroot_dir ] <> 0

let%test_unit "--create: exits 0 and produces DIR/root" =
  let rc = sudo [ oaur; "chroot"; "--create"; "-D"; chroot_dir ] in
  if rc <> 0 then failwith (Printf.sprintf "--create exited with %d" rc);
  let root = Filename.concat chroot_dir "root" in
  if not (Sys.file_exists root) then failwith (root ^ " should be a directory")

let%test_unit "--path: prints realpath of DIR/root" =
  let root = Filename.concat chroot_dir "root" in
  let expected = run_capture "realpath" [ "--"; root ] in
  let actual = run_capture oaur [ "chroot"; "--path"; "-D"; chroot_dir ] in
  if actual <> expected then
    failwith (Printf.sprintf "--path: expected %S, got %S" expected actual)

let%test_unit "--update: exits 0" =
  let rc = sudo [ oaur; "chroot"; "--update"; "-D"; chroot_dir ] in
  if rc <> 0 then failwith (Printf.sprintf "--update exited with %d" rc)

let%test_unit "--build: produces a .pkg.tar file from trivial PKGBUILD" =
  Unix.mkdir pkg_dir 0o755;
  Unix.mkdir pkgs_dir 0o755;
  let oc = open_out (Filename.concat pkg_dir "PKGBUILD") in
  output_string oc
    {|pkgname=oaur-test-hello
pkgver=1.0
pkgrel=1
arch=(any)
build() { true; }
package() {
  install -dm755 "$pkgdir/usr/share/$pkgname"
  printf 'hello\n' > "$pkgdir/usr/share/$pkgname/hello"
}
|};
  close_out oc;
  let saved = Unix.getcwd () in
  Unix.chdir pkg_dir;
  let env = Array.append (Unix.environment ()) [| "PKGDEST=" ^ pkgs_dir |] in
  let rc =
    run ~env "sudo"
      [ "--preserve-env=PKGDEST"; oaur; "chroot"; "--build"; "-D"; chroot_dir ]
  in
  Unix.chdir saved;
  if rc <> 0 then failwith (Printf.sprintf "--build exited with %d" rc);
  let has_pkg =
    Array.exists
      (fun f -> Str.string_match (Str.regexp ".*\\.pkg\\.tar") f 0)
      (Sys.readdir pkgs_dir)
  in
  if not has_pkg then failwith "expected a .pkg.tar file in PKGDEST"

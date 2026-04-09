open Core
open Aur.Utils

let oaur = "oaur"

let run ?(env = Core_unix.environment ()) cmd args =
  run_noexn ~suppress_output:false ~env (cmd, cmd :: args)
  |> exit_code_of_status

let run_capture cmd args = String.strip (run_read_all (cmd, cmd :: args))
let sudo args = run "sudo" args

(* shared chroot directory across tests — created once, cleaned up at exit *)
let tmpdir = Filename_unix.temp_dir "oaur-chroot-test" ""
let chroot_dir = Filename.concat tmpdir "aurbuild"
let pkg_dir = Filename.concat tmpdir "pkg"
let pkgs_dir = Filename.concat tmpdir "pkgs"

let () =
  at_exit (fun () ->
    ignore (sudo [ "umount"; "-R"; Filename.concat chroot_dir "root" ]);
    ignore (sudo [ "rm"; "-rf"; tmpdir ]))

(* --- tests --- *)

let%test "rejects missing action" = run oaur [ "chroot" ] <> 0

let%test "rejects multiple actions" =
  run oaur [ "chroot"; "--create"; "--path"; "-D"; chroot_dir ] <> 0

let%test_unit "--create: exits 0 and produces DIR/root" =
  [%test_eq: int] (sudo [ oaur; "chroot"; "--create"; "-D"; chroot_dir ]) 0;
  let root = Filename.concat chroot_dir "root" in
  [%test_pred: string] (fun p -> Sys.file_exists p |> phys_equal `Yes) root

let%test_unit "--path: prints realpath of DIR/root" =
  let root = Filename.concat chroot_dir "root" in
  [%test_eq: string]
    (run_capture oaur [ "chroot"; "--path"; "-D"; chroot_dir ])
    (run_capture "realpath" [ "--"; root ])

let%test_unit "--update: exits 0" =
  [%test_eq: int] (sudo [ oaur; "chroot"; "--update"; "-D"; chroot_dir ]) 0

let%test_unit "--build: produces a .pkg.tar file from trivial PKGBUILD" =
  Core_unix.mkdir pkg_dir ~perm:0o755;
  Core_unix.mkdir pkgs_dir ~perm:0o755;
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
  let saved = Core_unix.getcwd () in
  Core_unix.chdir pkg_dir;
  let env = Array.append (Core_unix.environment ()) [| "PKGDEST=" ^ pkgs_dir |] in
  let rc =
    run ~env "sudo"
      [ "--preserve-env=PKGDEST"; oaur; "chroot"; "--build"; "-D"; chroot_dir ]
  in
  Core_unix.chdir saved;
  [%test_eq: int] rc 0;
  [%test_pred: string array]
    (Array.exists (fun f -> Str.string_match (Str.regexp ".*\\.pkg\\.tar") f 0))
    (Sys.readdir pkgs_dir)

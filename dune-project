(lang dune 3.16)

(name oaur)

(generate_opam_files true)

(source
 (github fredericboileau/oaur))

(authors "Frederic Boileau")

(maintainers "Frederic Boileau")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name oaur)
 (synopsis "Archlinux User Repository helper")
 (description "Rewrite of aurutils functionalities in ocaml")
 (depends async ocaml dune core 
          core_unix uri cohttp-lwt-unix tls-lwt
          ocolor yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

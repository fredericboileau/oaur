(lang dune 3.16)

(name oaur)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oaur)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends async ocaml dune core 
          core_unix uri cohttp-lwt-unix atdgen
          ocolor yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

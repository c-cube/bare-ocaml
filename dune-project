(lang dune 2.0)
(generate_opam_files true)

(implicit_transitive_deps false)

(package
  (name bare)
  (synopsis "BARE encoding, see https://baremessages.org/")
  (tags (encoding binary bare))
  (license MIT)
  (depends dune (ocaml (>= "4.08"))))

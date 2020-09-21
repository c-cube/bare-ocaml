(lang dune 2.5)
(generate_opam_files true)
(name bare)

(implicit_transitive_deps false)

(package
  (name bare)
  (synopsis "BARE encoding, see https://baremessages.org/")
  (tags (encoding binary bare))
  (license MIT)
  (maintainers "simon cruanes")
  (authors "simon cruanes")
  (homepage "https://github.com/c-cube/bare-ocaml")
  (bug_reports "https://github.com/c-cube/bare-ocaml/issues")
  (depends
    (dune (>= "2.5"))
    (ocaml (>= "4.08"))
    (qtest (and :with-test (>= "2.10")))))

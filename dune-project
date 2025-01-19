(lang dune 2.7)

(name tezla-cfg)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/tezla-cfg.git"))

(bug_reports "https://github.com/joaosreis/tezla-cfg/issues")

(homepage "https://github.com/joaosreis/tezla-cfg")

(license MIT)

(authors "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(maintainers "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(package
 (name tezla-cfg)
 (synopsis "An CFG generator for Tezla programs")
 (depends
  (dune :build)
  michelson-adt
  michelson-parser
  tezla
  (core
   (and (>= v0.14)
   (< v0.15)))
  (core_unix
   (and (>= v0.14)
   (< v0.15)))
  ppx_deriving
  (graphlib (>= 2.5.0))
  cmdliner
  (alcotest :with-test)))

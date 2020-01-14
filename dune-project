(lang dune 1.11)
(name tezla-cfg)

(generate_opam_files true)

(source (uri "git+https://gitlab.com/releaselab/fresco/tezla-cfg.git"))
(bug_reports "https://gitlab.com/releaselab/fresco/tezla-cfg/issues")
(homepage "https://gitlab.com/releaselab/fresco/tezla-cfg")
(license MIT)
(authors "João Santos Reis <joaosreis@outlook.pt>")
(maintainers "João Santos Reis <joaosreis@outlook.pt>")

(package
 (name tezla-cfg)
 (synopsis "An CFG generator for Tezla programs")
 (depends
  (dune :build)
  (michelson :dev)
  (tezla :dev)
  (ocamlgraph :dev)
  (batteries :dev)))

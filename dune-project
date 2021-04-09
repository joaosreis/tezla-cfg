(lang dune 2.7)

(name tezla-cfg)

(generate_opam_files true)

(source
 (uri "git+https://gitlab.com/releaselab/fresco/tezla-cfg.git"))

(bug_reports "https://gitlab.com/releaselab/fresco/tezla-cfg/issues")

(homepage "https://gitlab.com/releaselab/fresco/tezla-cfg")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name tezla-cfg)
 (synopsis "An CFG generator for Tezla programs")
 (depends
  (dune :build)
  michelson
  tezla
  graphlib
  core_kernel
  ppx_deriving
  (alcotest :with-test)))

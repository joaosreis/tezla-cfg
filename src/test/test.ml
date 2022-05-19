open! Core

let () =
  let dir = "../../../../tests/" in
  let files = Sys_unix.readdir dir in
  let open Alcotest in
  let create_test filename =
    let convert_f () =
      let adt = Edo_parser.Parser.parse_file (dir ^ filename) in
      let adt = Edo_parser.Parser.convert (dir ^ filename) adt in
      let adt = Edo_adt.Typer.type_program adt in
      let tzl = Tezla.Converter.convert_program (ref (-1)) adt in
      let g = Tezla_cfg.Flow_graph.generate_from_program tzl in
      let () = Tezla_cfg.Flow_graph.dot_output g "cfg.dot" in
      check pass "Ok" () ()
    in
    test_case filename `Quick convert_f
  in
  let tests = Array.map files ~f:create_test in
  let tests = Array.to_list tests in
  run "Tezla CFG" [ ("generate", tests) ]

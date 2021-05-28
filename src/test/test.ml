open Core_kernel

let () =
  let dir = "../../../../tests/" in
  let files = Sys.readdir dir in
  let open Alcotest in
  let create_test filename =
    let open Michelson.Carthage.Parse in
    let convert_f () =
      match parse_program (dir ^ filename) with
      | Ok adt ->
          let adt, _ = program_parse adt in
          let tzl = Tezla.Converter.convert_program (ref (-1)) adt in
          let g = Tezla_cfg.Flow_graph.generate_from_program tzl in
          let () = Tezla_cfg.Flow_graph.dot_output g "cfg.dot" in
          check pass "Ok" () ()
      | Error e -> fail ("Generate CFG error: " ^ Error.to_string_hum e)
    in
    test_case filename `Quick convert_f
  in
  let tests = Array.map files ~f:create_test in
  let tests = Array.to_list tests in
  run "Tezla CFG" [ ("generate", tests) ]

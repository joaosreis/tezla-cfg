let () =
  let dir = "../../../../tests/" in
  let files = Sys.readdir dir in
  let open Alcotest in
  let create_test file =
    let open Michelson.Parser in
    let convert_f () =
      let adt = parse_file (dir ^ file) in
      let adt = convert adt in
      let tzl = Tezla.Converter.convert_program (ref (-1)) adt in
      let _ = Tezla_cfg.Flow_graph.generate_from_program tzl in
      ()
    in
    let test_f () =
      try
        convert_f ();
        check pass "Ok" () ()
      with Failure s -> fail ("Generate CFG error: " ^ s)
    in
    test_case file `Quick test_f
  in
  let tests = Array.map create_test files in
  let tests = Array.to_list tests in
  run "Tezla CFG" [ ("generate", tests) ]

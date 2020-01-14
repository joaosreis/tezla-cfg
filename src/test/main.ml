open Tezla
open Tezla_cfg

let path = Sys.argv.(1)

let () =
  try
    let adt = Parsing_utils.parse_with_error (path ^ "empty.tz") in
    let _ = Converter.convert_program adt in
    let cfg = Flow_graph.Cfg.generate_from_program adt in
    Flow_graph.Cfg.show cfg
  with e -> Printf.printf "Test empty: not OK | %s\n" (Printexc.to_string e)

let () =
  try
    let adt = Parsing_utils.parse_with_error (path ^ "empty_code.tz") in
    let _ = Converter.convert_program adt in
    let cfg = Flow_graph.Cfg.generate_from_program adt in
    Flow_graph.Cfg.show cfg
  with e ->
    Printf.printf "Test empty_code: not OK | %s\n" (Printexc.to_string e)

let () =
  try
    let adt = Parsing_utils.parse_with_error (path ^ "loop_left.tz") in
    let _ = Converter.convert_program adt in
    let cfg = Flow_graph.Cfg.generate_from_program adt in
    Flow_graph.Cfg.show cfg
  with e ->
    Printf.printf "Test loop_left: not OK | %s\n" (Printexc.to_string e)

let () =
  try
    let adt = Parsing_utils.parse_with_error (path ^ "map.tz") in
    let _ = Converter.convert_program adt in
    let cfg = Flow_graph.Cfg.generate_from_program adt in
    Flow_graph.Cfg.show cfg
  with e -> Printf.printf "Test map: not OK | %s\n" (Printexc.to_string e)

let tests = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

let () =
  List.iter
    (fun i ->
      try
        let adt =
          Parsing_utils.parse_with_error
            (path ^ "test" ^ string_of_int i ^ ".tz")
        in
        let _ = Converter.convert_program adt in
        let cfg = Flow_graph.Cfg.generate_from_program adt in
        Flow_graph.Cfg.show cfg
      with e ->
        Printf.printf "Test %d: not OK | %s\n" i (Printexc.to_string e))
    tests

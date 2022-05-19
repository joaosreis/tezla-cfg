open! Core
open Cmdliner

let cfg_dot f_in f_out =
  let nodes = Edo_parser.Parser.parse_file f_in in
  let adt = Edo_parser.Parser.convert f_in nodes in
  let adt = Edo_adt.Typer.type_program adt in
  let tzl = Tezla.Converter.convert_program (ref (-1)) adt in
  let cfg = Tezla_cfg.Flow_graph.generate_from_program tzl in
  Tezla_cfg.Flow_graph.dot_output cfg f_out

let command =
  let input =
    let doc = "Michelson source code file" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"IN" ~doc)
  in
  let out =
    let doc = "Output file" in
    Arg.(
      value & opt string "cfg.dot" & info [ "o"; "output" ] ~docv:"DEST" ~doc)
  in
  let doc = "Generate Tezla CFG dot output from a Michelson smart contract" in
  let exits = Cmd.Exit.defaults in
  let term = Term.(const cfg_dot $ input $ out) in
  Cmd.v (Cmd.info "dot" ~doc ~exits) term

let () = Stdlib.exit (Cmd.eval command)

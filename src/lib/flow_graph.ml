open Core_kernel
open Graphlib.Std

type program = Michelson.Carthage.Adt.program

type expr = Cfg_node.expr

type var = Cfg_node.var

type node = Cfg_node.t [@@deriving ord, sexp]

module G = Graphlib.Make (Cfg_node) (Edge)

type t = {
  graph : G.t;
  mutable initial : Cfg_node.t;
  finals : Set.M(Cfg_node).t;
  vars : Cfg_node.Var.Set.t;
}

let empty =
  {
    graph = G.empty;
    initial = { label = -1; stmt = Cfg_node.Cfg_skip };
    finals = Set.empty (module Cfg_node);
    vars = Cfg_node.Var.Set.empty;
  }

let get g l = Sequence.find (G.nodes g.graph) ~f:(Cfg_node.equal l)

let nodes g = G.nodes g.graph

let connect g e (n_1 : Cfg_node.t) (n_2 : Cfg_node.t) =
  let vars = g.vars in
  let vars =
    match n_1.stmt with
    | Cfg_assign (v, _) -> Cfg_node.Var.Set.add vars v
    | _ -> vars
  in
  let vars =
    match n_2.stmt with
    | Cfg_assign (v, _) -> Cfg_node.Var.Set.add vars v
    | _ -> vars
  in
  let edge = G.Edge.create n_1 n_2 e in
  { g with graph = G.Edge.insert edge g.graph; vars }

let is_initial g v = Cfg_node.equal g.initial v

let is_final g v = Set.exists g.finals ~f:(Cfg_node.equal v)

let dot_output g filename =
  Graphlib.to_dot
    (module G)
    ~node_attrs:(fun n -> [ `Label (Cfg_node.to_string n) ])
    ~edge_attrs:(fun e ->
      match G.Edge.label e with
      | Normal -> []
      | True -> [ `Label "true" ]
      | False -> [ `Label "false" ])
    ~filename g.graph

let generate_from_program p =
  let graph = empty in
  let add_edge graph ((e, i, j) : Flow.edge) = connect graph e i j in
  let open Flow in
  let _, _, code = p in
  let { flow; initial; finals; _ } = flow code in
  let graph = Set.fold ~f:add_edge flow ~init:graph in
  { graph with initial; finals }

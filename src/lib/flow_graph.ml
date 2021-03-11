type program =
  (Michelson.Location.t, Michelson.Adt.annot list) Michelson.Adt.program

type expr = Cfg_node.expr

type var = Cfg_node.var

module V = struct
  include Int

  let hash = Hashtbl.hash
end

type vertex = V.t

module G = Graph.Persistent.Digraph.ConcreteBidirectional (V)
module Node_map = Map.Make (Int)

type t = {
  flow : G.t;
  blocks : Cfg_node.t Node_map.t;
  initial : vertex;
  finals : vertex Batteries.Set.t;
  vars : var list;
}

module Display (X : sig
  val label_to_subgraph : vertex -> Graph.Graphviz.DotAttributes.subgraph

  val label_to_dot_label : vertex -> string
end) =
struct
  include G

  let vertex_name = string_of_int

  let graph_attributes _ = []

  let default_vertex_attributes _ = [ `Shape `Box; `Fontname "Courier" ]

  let vertex_attributes v = [ `Label (X.label_to_dot_label v) ]

  let default_edge_attributes _ = []

  let edge_attributes _ = []

  let get_subgraph v = Some (X.label_to_subgraph v)
end

let empty =
  {
    flow = G.empty;
    blocks = Node_map.empty;
    initial = 0;
    finals = Batteries.Set.empty;
    vars = [];
  }

let inflow g = G.pred g.flow

let outflow g = G.succ g.flow

let add g v =
  let blocks = Node_map.add v.Cfg_node.id v g.blocks in
  let flow = G.add_vertex g.flow v.Cfg_node.id in
  let vars =
    match v.stmt with Cfg_assign (v, _) -> v :: g.vars | _ -> g.vars
  in
  { g with flow; blocks; vars }

let get g i = Node_map.find i g.blocks

let get_blocks g = Node_map.fold (fun _ v acc -> v :: acc) g.blocks []

let labels g = Node_map.fold (fun _ v acc -> v.Cfg_node.id :: acc) g.blocks []

let connect g v_1 v_2 = { g with flow = G.add_edge g.flow v_1 v_2 }

let is_initial g v = g.initial = v

let is_final g v = Batteries.Set.exists (( = ) v) g.finals

let dot_output g f =
  let module Helper = struct
    let label_to_dot_label v =
      Format.sprintf "%d: %a" v
        (fun () n ->
          let s =
            Pp.pp_node Format.str_formatter n;
            Format.flush_str_formatter ()
          in
          String.escaped s)
        (Node_map.find v g.blocks)

    let label_to_subgraph _ =
      {
        Graph.Graphviz.DotAttributes.sg_name = "";
        sg_attributes = [ `Label "" ];
        sg_parent = None;
      }
  end in
  let module Dot_ = Graph.Graphviz.Dot (Display (Helper)) in
  let oc = open_out f in
  Dot_.output_graph oc g.flow;
  close_out oc

let display_with_gv g =
  let tmp_dot = Filename.temp_file "graph" ".dot" in
  dot_output g tmp_dot;
  let tmp_ps = Filename.temp_file "graph" ".ps" in
  ignore
    (Sys.command
       ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps ^ " &"));
  Sys.remove tmp_dot

let show = display_with_gv

let generate_from_program p =
  let open Batteries in
  let graph = empty in
  let add_edge (i, j) graph = connect graph i j in
  let open Flow in
  let _, _, code = p in
  let { nodes; flow; initial; finals; _ } = flow code in
  let graph = Hashtbl.fold (fun _ b graph -> add graph b) nodes graph in
  let graph = Set.fold add_edge flow graph in
  { graph with initial; finals }

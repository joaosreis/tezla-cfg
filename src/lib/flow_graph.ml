module Common = struct
  type program =
    (Michelson.Location.t, Michelson.Adt.annot list) Michelson.Adt.program

  type expr = Cfg_node.expr

  module V = struct
    type t = Cfg_node.t

    let equal v_1 v_2 = v_1.Cfg_node.id = v_2.Cfg_node.id

    let compare v_1 v_2 = compare v_1.Cfg_node.id v_2.Cfg_node.id

    let hash v = Hashtbl.hash v.Cfg_node.id
  end

  type vertex = V.t

  module G = Graph.Persistent.Digraph.ConcreteBidirectional (V)

  module Display (X : sig
    val label_to_subgraph : vertex -> Graph.Graphviz.DotAttributes.subgraph

    val label_to_dot_label : vertex -> string
  end) =
  struct
    include G

    let vertex_name v = string_of_int v.Cfg_node.id

    let graph_attributes _ = []

    let default_vertex_attributes _ = [ `Shape `Box; `Fontname "Courier" ]

    let vertex_attributes v = [ `Label (X.label_to_dot_label v) ]

    let default_edge_attributes _ = []

    let edge_attributes _ = []

    let get_subgraph v = Some (X.label_to_subgraph v)
  end
end

module Cfg = struct
  include Common
  module Node_map = Map.Make (Int)

  type t = { flow : G.t; blocks : V.t Node_map.t }

  let create () = { flow = G.empty; blocks = Node_map.empty }

  let inflow g = G.pred g.flow

  let outflow g = G.succ g.flow

  let add g v =
    let blocks = Node_map.add v.Cfg_node.id v g.blocks in
    let flow = G.add_vertex g.flow v in
    { flow; blocks }

  let get g i = Node_map.find i g.blocks

  let get_blocks g = Node_map.fold (fun _ v acc -> v :: acc) g.blocks []

  let connect g v_1 v_2 = { g with flow = G.add_edge g.flow v_1 v_2 }

  let dot_output g f =
    let module Helper = struct
      let label_to_dot_label v =
        Printf.sprintf "%d: %s" v.Cfg_node.id (Cfg_node.to_string v)

      let label_to_subgraph _ =
        {
          Graph.Graphviz.DotAttributes.sg_name = "";
          sg_attributes = [ `Label "" ];
          sg_parent = None;
        }
    end in
    let module Dot_ = Graph.Graphviz.Dot (Display (Helper)) in
    let oc = open_out f in
    Dot_.output_graph oc g;
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
    let graph = create () in
    let counter = ref (-1) in
    let p =
      let open Michelson.Adt in
      {
        code = Tezla.Converter.inst_strip_location p.code;
        param = Tezla.Converter.typ_strip_location p.param;
        storage = Tezla.Converter.typ_strip_location p.storage;
      }
    in
    let p_tezla = Tezla.Converter.convert_program counter p in
    let add_edge (i, j) graph = connect graph i j in
    let open Flow in
    let _, _, code = p_tezla in
    let { nodes; flow; init_ht; final_ht; _ } = flow counter code in
    let graph = Set.fold (fun b graph -> add graph b) nodes graph in
    let graph = Set.fold add_edge flow graph in
    graph
end

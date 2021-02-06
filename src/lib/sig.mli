open Batteries

module type Flow = sig
  type block

  type vertex

  type t = {
    correspondence : (block, vertex) Hashtbl.t;
    nodes : vertex Set.t;
    initial : vertex Set.t;
    final : vertex Set.t;
    flow : (vertex * vertex) Set.t;
  }

  val init : block -> block

  val final : block -> block Set.t

  val flow : block -> t

  val flowR : block -> t
end

module type Flow_graph = sig
  type expr

  type vertex

  type program

  type t

  val create : unit -> t

  val inflow : t -> int -> int list

  val outflow : t -> int -> int list

  val add : t -> string -> vertex -> t

  val get : t -> int -> vertex

  val connect : t -> vertex -> vertex -> t

  val get_blocks : t -> (int, vertex) Hashtbl.t

  val dot_output : t -> string -> unit

  val display_with_gv : t -> unit

  val show : t -> unit

  val generate_from_program : program -> t
end

module type Inter_flow_graph = sig
  include Flow_graph

  val inter_flow : t -> (int * int * int * int) list

  (* TODO: val callees : t -> int -> int list *)
end

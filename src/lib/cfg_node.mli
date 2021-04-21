open Core_kernel

type loc = Unknown | Loc of int * int

module Var : module type of Tezla.Adt.Var

type var = Var.t [@@deriving ord, sexp]

type ident = var [@@deriving ord, sexp]

type decl = ident [@@deriving ord, sexp]

module Typ : module type of Tezla.Adt.Typ

type typ = Typ.t [@@deriving ord, sexp]

module Expr : module type of Tezla.Adt.Expr

type expr = Expr.t [@@deriving ord, sexp]

type stmt =
  | Cfg_assign of var * expr
  | Cfg_skip
  | Cfg_drop of var list
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of var
  | Cfg_if_none of var
  | Cfg_if_left of var
  | Cfg_if_cons of var
  | Cfg_loop of var
  | Cfg_loop_left of var
  | Cfg_map of var
  | Cfg_iter of var
  | Cfg_failwith of var
  | Cfg_return of var

type t = { label : int; stmt : stmt } [@@deriving sexp]

include Comparable.S with type t := t

include Regular.Std.Opaque.S with type t := t

val compare : t -> t -> int

val create_node : ?label:int -> stmt -> t

val to_string : t -> string

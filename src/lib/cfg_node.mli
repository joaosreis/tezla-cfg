type loc = Unknown | Loc of int * int

type var = Tezla.Adt.var

type ident = Tezla.Adt.var

type decl = ident

type typ = Tezla.Adt.typ

type expr = Tezla.Adt.expr

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

type t = { id : int; stmt : stmt }

val to_string : t -> string

val create_node : ?id:int -> stmt -> t

type loc = Unknown | Loc of int * int

type ident = string

type decl = ident

type typ = Tezla.Adt.typ

type expr = Tezla.Adt.expr

type stmt =
  | Cfg_var_decl of string * typ option
  | Cfg_assign of string * expr
  | Cfg_decl_assign of string * expr * typ option
  | Cfg_skip
  | Cfg_drop of Z.t
  | Cfg_swap
  | Cfg_dig
  | Cfg_dug
  | Cfg_if of string
  | Cfg_if_none of string * string
  | Cfg_if_left of string * string
  | Cfg_if_cons of string * string * string
  | Cfg_loop of string
  | Cfg_loop_left of string
  | Cfg_map of string
  | Cfg_iter of string
  | Cfg_failwith of string
  | Cfg_cast
  | Cfg_contract of typ

type t = { id : int; stmt : stmt }

val to_string : t -> string

val create_node : stmt -> t

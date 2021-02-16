open Batteries

type loc = Unknown | Loc of int * int

type var = Tezla.Adt.var

type ident = var

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
  | Cfg_return of var

type t = { id : int; stmt : stmt }

let id_counter = ref (-1)

let next_id () =
  let () = id_counter := !id_counter + 1 in
  !id_counter

let create_node ?id stmt =
  let id = match id with None -> next_id () | Some id -> id in
  { id; stmt }

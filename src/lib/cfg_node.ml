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

type t = { id : int; stmt : stmt }

let to_string n =
  let open Printf in
  let open Tezla.Adt in
  let open Tezla.Pp in
  match n.stmt with
  | Cfg_assign (v, e) -> sprintf "%s := %s" v.var_name (string_of_expr e)
  | Cfg_skip -> "skip"
  | Cfg_drop l -> sprintf "DROP %s" (string_of_list (fun v -> v.var_name) l)
  | Cfg_swap -> "SWAP"
  | Cfg_dig -> "DIG"
  | Cfg_dug -> "DUG"
  | Cfg_if v -> sprintf "IF %s" v.var_name
  | Cfg_if_none v -> sprintf "IF_NONE %s" v.var_name
  | Cfg_if_left v -> sprintf "IF_LEFT %s" v.var_name
  | Cfg_if_cons v -> sprintf "IF_CONS %s" v.var_name
  | Cfg_loop v -> sprintf "LOOP %s" v.var_name
  | Cfg_loop_left v -> sprintf "LOOP_LEFT %s" v.var_name
  | Cfg_map v -> sprintf "MAP %s" v.var_name
  | Cfg_iter v -> sprintf "ITER %s" v.var_name
  | Cfg_failwith v -> sprintf "FAILWITH %s" v.var_name

let id_counter = ref (-1)

let next_id () =
  let () = id_counter := !id_counter + 1 in
  !id_counter

let create_node ?id stmt =
  let id = match id with None -> next_id () | Some id -> id in
  { id; stmt }

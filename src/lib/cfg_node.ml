open Batteries

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

let to_string n =
  let open Format in
  let open Tezla.Pp in
  let () =
    match n.stmt with
    | Cfg_var_decl (s, None) -> fprintf str_formatter "var %s" s
    | Cfg_var_decl (s, Some t) ->
        fprintf str_formatter "var %s : %a" s typ (fst t)
    | Cfg_assign (s, e) -> fprintf str_formatter "%s := %a" s Tezla.Pp.expr e
    | Cfg_decl_assign (s, e, None) ->
        fprintf str_formatter "var %s := %a" s expr e
    | Cfg_decl_assign (s, e, Some t) ->
        fprintf str_formatter "var %s : %a := %a" s typ (fst t) expr e
    | Cfg_skip -> fprintf str_formatter "skip"
    | Cfg_drop n ->
        if Z.(n = one) then fprintf str_formatter "DROP"
        else fprintf str_formatter "DROP %a" Z.pp_print n
    | Cfg_swap -> fprintf str_formatter "SWAP"
    | Cfg_dig -> fprintf str_formatter "DIG"
    | Cfg_dug -> fprintf str_formatter "DUG"
    | Cfg_if s -> fprintf str_formatter "IF %s" s
    | Cfg_if_none (s, _) -> fprintf str_formatter "IF_NONE %s" s
    | Cfg_if_left (s, _) -> fprintf str_formatter "IF_LEFT %s" s
    | Cfg_if_cons (s, _, _) -> fprintf str_formatter "IF_CONS %s" s
    | Cfg_loop s -> fprintf str_formatter "LOOP %s" s
    | Cfg_loop_left s -> fprintf str_formatter "LOOP_LEFT %s" s
    | Cfg_map s -> fprintf str_formatter "MAP %s" s
    | Cfg_iter s -> fprintf str_formatter "ITER %s" s
    | Cfg_failwith s -> fprintf str_formatter "FAILWITH %s" s
    | Cfg_cast -> fprintf str_formatter "CAST"
    | Cfg_contract t -> fprintf str_formatter "CONTRACT %a" typ (fst t)
  in
  flush_str_formatter ()

let id_counter = ref (-1)

let next_id () =
  let () = id_counter := !id_counter + 1 in
  !id_counter

let create_node stmt =
  let id = next_id () in
  { id; stmt }

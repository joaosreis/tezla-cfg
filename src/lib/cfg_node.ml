open! Core

type loc = Unknown | Loc of int * int

module Var = Tezla.Adt.Var

type var = Var.t [@@deriving ord, sexp]
type ident = var [@@deriving ord, sexp]
type decl = ident [@@deriving ord, sexp]

module Typ = Edo_adt.Adt.Typ

type typ = Typ.t [@@deriving ord, sexp]

module Expr = Tezla.Adt.Expr

type expr = Expr.t [@@deriving sexp]

type stmt =
  | Cfg_assign of var * expr
  | Cfg_skip
  | Cfg_drop of var list
  | Cfg_swap
  | Cfg_dig of Bigint.t
  | Cfg_dug of Bigint.t
  | Cfg_if of var
  | Cfg_if_none of var
  | Cfg_if_left of var
  | Cfg_if_cons of var
  | Cfg_loop of var
  | Cfg_loop_left of var
  | Cfg_map_list of var
  | Cfg_map_map of var
  | Cfg_iter_list of var
  | Cfg_iter_set of var
  | Cfg_iter_map of var
  | Cfg_failwith of var
  | Cfg_return of var
[@@deriving sexp]

module T = struct
  type t = { label : int; stmt : stmt [@hash.ignore] } [@@deriving sexp, hash]

  let to_string s =
    let open Tezla.Adt in
    let stmt_string =
      match s.stmt with
      | Cfg_assign (v, e) -> (
          [%string "%{v.Var.var_name} : %{(v.Var.var_type)#Edo_adt.Typ} := "]
          ^
          match e.value with
          | Tezla.Adt.E_lambda _ -> "LAMBDA { ... }"
          | Tezla.Adt.E_push d -> [%string "PUSH %{d#Data}"]
          | _ -> Expr.to_string e)
      | Cfg_skip -> "skip"
      | Cfg_drop l -> [%string "DROP %{List.to_string ~f:Var.to_string l}"]
      | Cfg_swap -> "SWAP"
      | Cfg_dig n -> [%string "DIG %{n#Bigint}"]
      | Cfg_dug n -> [%string "DUG %{n#Bigint}"]
      | Cfg_if v -> [%string "IF %{v#Var}"]
      | Cfg_if_none v -> [%string "IF_NONE %{v#Var}"]
      | Cfg_if_left v -> [%string "IF_LEFT %{v#Var}"]
      | Cfg_if_cons v -> [%string "IF_CONS %{v#Var}"]
      | Cfg_loop v -> [%string "LOOP %{v#Var}"]
      | Cfg_loop_left v -> [%string "LOOP_LEFT %{v#Var}"]
      | Cfg_map_list v -> [%string "MAP_list %{v#Var}"]
      | Cfg_map_map v -> [%string "MAP_map %{v#Var}"]
      | Cfg_iter_list v -> [%string "ITER_list %{v#Var}"]
      | Cfg_iter_set v -> [%string "ITER_set %{v#Var}"]
      | Cfg_iter_map v -> [%string "ITER_map %{v#Var}"]
      | Cfg_failwith v -> [%string "FAILWITH %{v#Var}"]
      | Cfg_return v -> [%string "return %{v#Var}"]
    in
    [%string "%{s.label#Int}: %{stmt_string}"]

  let compare n_1 n_2 = Int.compare n_1.label n_2.label
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let label_counter = ref (-1)

let next_label () =
  let () = label_counter := !label_counter + 1 in
  !label_counter

let create_node ?label stmt =
  let label = match label with None -> next_label () | Some label -> label in
  { label; stmt }

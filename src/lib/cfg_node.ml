open Core_kernel

type loc = Unknown | Loc of int * int

type var = Tezla.Adt.Var.t [@@deriving ord, sexp]

type ident = var

type decl = ident

type typ = Tezla.Adt.typ

type expr = Tezla.Adt.expr [@@deriving ord, sexp]

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
[@@deriving sexp]

module T = struct
  type t = { label : int; stmt : stmt } [@@deriving sexp]

  let hash x = Int.hash x.label

  let to_string s =
    let open Tezla.Adt in
    let stmt_string =
      match s.stmt with
      | Cfg_assign (v, e) -> (
          match e with
          | Tezla.Adt.E_lambda _ ->
              [%string "%{v#Tezla.Adt.Var} := LAMBDA { ... }"]
          | Tezla.Adt.E_push (d, t) ->
              [%string "%{v#Var} := PUSH %{t#Typ} %{d#Data}"]
          | _ -> [%string "%{v#Var} := %{e#Expr}"] )
      | Cfg_skip -> "skip"
      | Cfg_drop l -> [%string "DROP %{List.to_string ~f:Var.to_string l}"]
      | Cfg_swap -> "SWAP"
      | Cfg_dig -> "DIG"
      | Cfg_dug -> "DUG"
      | Cfg_if v -> [%string "IF %{v#Var}"]
      | Cfg_if_none v -> [%string "IF_NONE %{v#Var}"]
      | Cfg_if_left v -> [%string "IF_LEFT %{v#Var}"]
      | Cfg_if_cons v -> [%string "IF_CONS %{v#Var}"]
      | Cfg_loop v -> [%string "LOOP %{v#Var}"]
      | Cfg_loop_left v -> [%string "LOOP_LEFT %{v#Var}"]
      | Cfg_map v -> [%string "MAP %{v#Var}"]
      | Cfg_iter v -> [%string "ITER %{v#Var}"]
      | Cfg_failwith v -> [%string "FAILWITH %{v#Var}"]
      | Cfg_return v -> [%string "return %{v#Var}"]
    in
    [%string "%{s.label#Int}: %{stmt_string}"]

  let compare n_1 n_2 = Int.compare n_1.label n_2.label
end

include T
include Comparable.Make (T)
include Regular.Std.Opaque.Make (T)

let label_counter = ref (-1)

let next_label () =
  let () = label_counter := !label_counter + 1 in
  !label_counter

let create_node ?label stmt =
  let label = match label with None -> next_label () | Some label -> label in
  { label; stmt }

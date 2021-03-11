open Format

let pp_node ppf n =
  let open Tezla.Pp in
  let pp_data ppf =
    let open Tezla.Adt in
    function
    | D_instruction _ -> fprintf ppf "{ ... }"
    | D_pair (d_1, d_2) -> fprintf ppf "(Pair %a %a)" pp_data d_1 pp_data d_2
    | D_left d -> fprintf ppf "(Left %a)" pp_data d
    | D_right d -> fprintf ppf "(Right %a)" pp_data d
    | D_some d -> fprintf ppf "(Some %a)" pp_data d
    | D_elt (d_1, d_2) -> fprintf ppf "(Elt %a %a)" pp_data d_1 pp_data d_2
    | D_list d_l -> List.iter (pp_data ppf) d_l
    | d -> Tezla.Pp.pp_data ppf d
  in
  let open Cfg_node in
  match n.stmt with
  | Cfg_assign (v, e) -> (
      match e with
      | Tezla.Adt.E_lambda _ -> fprintf ppf "%a := LAMBDA { ... }" pp_var v
      | Tezla.Adt.E_push (d, t) ->
          fprintf ppf "%a := PUSH %a %a" pp_var v pp_typ t pp_data d
      | _ -> fprintf ppf "%a := %a" pp_var v pp_expr e )
  | Cfg_skip -> fprintf ppf "skip"
  | Cfg_drop l ->
      fprintf ppf "DROP %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf " ")
           (fun ppf v -> fprintf ppf "%a" pp_var v))
        l
  | Cfg_swap -> fprintf ppf "SWAP"
  | Cfg_dig -> fprintf ppf "DIG"
  | Cfg_dug -> fprintf ppf "DUG"
  | Cfg_if v -> fprintf ppf "IF %a" pp_var v
  | Cfg_if_none v -> fprintf ppf "IF_NONE %a" pp_var v
  | Cfg_if_left v -> fprintf ppf "IF_LEFT %a" pp_var v
  | Cfg_if_cons v -> fprintf ppf "IF_CONS %a" pp_var v
  | Cfg_loop v -> fprintf ppf "LOOP %a" pp_var v
  | Cfg_loop_left v -> fprintf ppf "LOOP_LEFT %a" pp_var v
  | Cfg_map v -> fprintf ppf "MAP %a" pp_var v
  | Cfg_iter v -> fprintf ppf "ITER %a" pp_var v
  | Cfg_failwith v -> fprintf ppf "FAILWITH %a" pp_var v
  | Cfg_return v -> fprintf ppf "return %a" pp_var v

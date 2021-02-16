open Format

let pp_node ppf n =
  let open Tezla.Pp in
  let open Cfg_node in
  match n.stmt with
  | Cfg_assign (v, e) -> fprintf ppf "%a := %a" pp_var v pp_expr e
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

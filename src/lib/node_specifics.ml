open Batteries

let find_assignments blocks x =
  let open Cfg_node in
  let aux b acc =
    match b with
    | Cfg_assign (lv, _) when lv = x -> Set.add b acc
    | Cfg_assign _ | Cfg_dig | Cfg_drop _ | Cfg_failwith _ | Cfg_if_cons _
    | Cfg_if_left _ | Cfg_if_none _ | Cfg_skip | Cfg_swap | Cfg_dug | Cfg_if _
    | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ | Cfg_return _ ->
        acc
  in
  Set.fold aux blocks Set.empty

let free_variables =
  let free_variables_expr acc =
    let open Tezla.Adt in
    function
    | E_unit | E_self | E_now | E_amount | E_balance | E_source | E_sender
    | E_none _
    | E_push (_, _)
    | E_nil _ | E_empty_set _
    | E_empty_map (_, _)
    | E_empty_big_map (_, _)
    | E_lambda (_, _, _, _)
    | E_special_empty_list _
    | E_special_empty_map (_, _)
    | E_chain_id ->
        acc
    | E_car v
    | E_cdr v
    | E_neg v
    | E_not v
    | E_eq v
    | E_abs v
    | E_neq v
    | E_lt v
    | E_gt v
    | E_leq v
    | E_geq v
    | E_left (v, _)
    | E_right (v, _)
    | E_pack v
    | E_blake2b v
    | E_sha256 v
    | E_sha512 v
    | E_hash_key v
    | E_address_of_contract v
    | E_unpack (_, v)
    | E_contract_of_address (_, v)
    | E_concat_list v
    | E_implicit_account v
    | E_unlift_option v
    | E_unlift_or_left v
    | E_unlift_or_right v
    | E_hd v
    | E_tl v
    | E_size v
    | E_isnat v
    | E_int_of_nat v
    | E_dup v
    | E_some v ->
        Set.add v acc
    | E_operation o -> (
        match o with
        | O_create_account (v_1, v_2, v_3, v_4) ->
            Set.add v_4 (Set.add v_3 (Set.add v_2 (Set.add v_1 acc)))
        | O_create_contract (_, v_1, v_2, v_3)
        | O_transfer_tokens (v_1, v_2, v_3) ->
            Set.add v_3 (Set.add v_2 (Set.add v_1 acc))
        | O_set_delegate v -> Set.add v acc )
    | E_add (v_1, v_2)
    | E_sub (v_1, v_2)
    | E_mul (v_1, v_2)
    | E_div (v_1, v_2)
    | E_shiftL (v_1, v_2)
    | E_shiftR (v_1, v_2)
    | E_and (v_1, v_2)
    | E_or (v_1, v_2)
    | E_xor (v_1, v_2)
    | E_compare (v_1, v_2)
    | E_cons (v_1, v_2)
    | E_pair (v_1, v_2)
    | E_mem (v_1, v_2)
    | E_concat (v_1, v_2)
    | E_apply (v_1, v_2)
    | E_append (v_1, v_2)
    | E_exec (v_1, v_2)
    | E_phi (v_1, v_2)
    | E_get (v_1, v_2) ->
        Set.add v_2 (Set.add v_1 acc)
    | E_update (v_1, v_2, v_3)
    | E_slice (v_1, v_2, v_3)
    | E_create_contract_address (_, v_1, v_2, v_3)
    | E_check_signature (v_1, v_2, v_3) ->
        Set.add v_3 (Set.add v_2 (Set.add v_1 acc))
  in
  let open Cfg_node in
  function
  | Cfg_assign (_, rv) -> free_variables_expr Set.empty rv
  | Cfg_skip -> Set.empty
  | Cfg_swap -> Set.empty
  | Cfg_dig -> Set.empty
  | Cfg_dug -> Set.empty
  | Cfg_drop v_l -> Set.union Set.empty (Set.of_list v_l)
  | Cfg_if v -> Set.add v Set.empty
  | Cfg_if_none v
  | Cfg_if_cons v
  | Cfg_loop v
  | Cfg_loop_left v
  | Cfg_map v
  | Cfg_iter v
  | Cfg_failwith v
  | Cfg_return v
  | Cfg_if_left v ->
      Set.add v Set.empty

open! Core

let find_assignments blocks x =
  let aux acc = function
    | Cfg_node.Cfg_assign (lv, _) when Cfg_node.compare_var lv x = 0 ->
        Set.add acc lv
    | Cfg_assign _ | Cfg_dig _ | Cfg_drop _ | Cfg_failwith _ | Cfg_if_cons _
    | Cfg_if_left _ | Cfg_if_none _ | Cfg_skip | Cfg_swap | Cfg_dug _ | Cfg_if _
    | Cfg_loop _ | Cfg_loop_left _ | Cfg_map_list _ | Cfg_map_map _
    | Cfg_iter_list _ | Cfg_iter_map _ | Cfg_iter_set _ | Cfg_return _ ->
        acc
  in
  Set.fold blocks ~init:Cfg_node.Var.Set.empty ~f:aux

let free_variables =
  let module Set = Cfg_node.Var.Set in
  let free_variables_expr acc e =
    let open Tezla.Adt in
    match e.Node.value with
    | E_unit | E_self | E_now | E_amount | E_balance | E_source | E_sender
    | E_none _ | E_push _ | E_nil _ | E_empty_set _
    | E_empty_map (_, _)
    | E_empty_big_map (_, _)
    | E_lambda (_, _, _)
    | E_special_empty_list _
    | E_special_empty_map (_, _)
    | E_chain_id | E_total_voting_power | E_self_address | E_level
    | E_sapling_empty_state _ ->
        acc
    | E_car v
    | E_cdr v
    | E_neg_bls12_381_fr v
    | E_neg_bls12_381_g1 v
    | E_neg_bls12_381_g2 v
    | E_neg_int v
    | E_neg_nat v
    | E_not_bool v
    | E_not_int v
    | E_not_nat v
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
    | E_concat_list_string v
    | E_concat_list_bytes v
    | E_implicit_account v
    | E_unlift_option v
    | E_unlift_or_left v
    | E_unlift_or_right v
    | E_hd v
    | E_tl v
    | E_isnat v
    | E_int_of_nat v
    | E_dup v
    | E_some v
    | E_var v
    | E_size_bytes v
    | E_size_string v
    | E_size_list v
    | E_size_map v
    | E_size_set v
    | E_voting_power v
    | E_keccak v
    | E_sha3 v
    | E_pairing_check v
    | E_read_ticket_pair v
    | E_read_ticket_ticket v
    | E_join_ticket v
    | E_get_n (_, v)
    | E_dup_n (_, v) ->
        Set.add acc v
    | E_operation o -> (
        match o with
        | O_create_account (v_1, v_2, v_3, v_4) ->
            Set.add (Set.add (Set.add (Set.add acc v_1) v_2) v_3) v_4
        | O_create_contract (_, v_1, v_2, v_3)
        | O_transfer_tokens (v_1, v_2, v_3) ->
            Set.add (Set.add (Set.add acc v_1) v_2) v_3
        | O_set_delegate v -> Set.add acc v)
    | E_add_bls12_381_fr (v_1, v_2)
    | E_add_bls12_381_g1 (v_1, v_2)
    | E_add_bls12_381_g2 (v_1, v_2)
    | E_add_int (v_1, v_2)
    | E_add_mutez (v_1, v_2)
    | E_add_nat (v_1, v_2)
    | E_add_nat_int (v_1, v_2)
    | E_add_timestamp_int (v_1, v_2)
    | E_sub_int (v_1, v_2)
    | E_sub_nat (v_1, v_2)
    | E_sub_nat_int (v_1, v_2)
    | E_sub_mutez (v_1, v_2)
    | E_sub_timestamp (v_1, v_2)
    | E_sub_timestamp_int (v_1, v_2)
    | E_mul_bls12_381_fr_bls12_381_fr (v_1, v_2)
    | E_mul_bls12_381_g1_bls12_381_fr (v_1, v_2)
    | E_mul_bls12_381_g2_bls12_381_fr (v_1, v_2)
    | E_mul_int (v_1, v_2)
    | E_mul_nat (v_1, v_2)
    | E_mul_nat_int (v_1, v_2)
    | E_mul_mutez_nat (v_1, v_2)
    | E_mul_int_bls12_381_fr (v_1, v_2)
    | E_mul_nat_bls12_381_fr (v_1, v_2)
    | E_ediv_int (v_1, v_2)
    | E_ediv_nat (v_1, v_2)
    | E_ediv_nat_int (v_1, v_2)
    | E_ediv_mutez (v_1, v_2)
    | E_ediv_mutez_nat (v_1, v_2)
    | E_lsl (v_1, v_2)
    | E_lsr (v_1, v_2)
    | E_and_bool (v_1, v_2)
    | E_and_int_nat (v_1, v_2)
    | E_and_nat (v_1, v_2)
    | E_or_bool (v_1, v_2)
    | E_or_nat (v_1, v_2)
    | E_xor_bool (v_1, v_2)
    | E_xor_nat (v_1, v_2)
    | E_compare (v_1, v_2)
    | E_cons (v_1, v_2)
    | E_pair (v_1, v_2)
    | E_mem_set (v_1, v_2)
    | E_mem_map (v_1, v_2)
    | E_mem_big_map (v_1, v_2)
    | E_concat_bytes (v_1, v_2)
    | E_concat_string (v_1, v_2)
    | E_apply (v_1, v_2)
    | E_list_append (v_1, v_2)
    | E_exec (v_1, v_2)
    | E_get_map (v_1, v_2)
    | E_get_big_map (v_1, v_2)
    | E_sapling_verify_update (v_1, v_2)
    | E_ticket (v_1, v_2)
    | E_split_ticket (v_1, v_2)
    | E_update_n (_, v_1, v_2) ->
        Set.add (Set.add acc v_1) v_2
    | E_update_set (v_1, v_2, v_3)
    | E_update_map (v_1, v_2, v_3)
    | E_update_big_map (v_1, v_2, v_3)
    | E_slice_bytes (v_1, v_2, v_3)
    | E_slice_string (v_1, v_2, v_3)
    | E_create_contract_address (_, v_1, v_2, v_3)
    | E_open_chest (v_1, v_2, v_3)
    | E_get_and_update_val (v_1, v_2, v_3)
    | E_get_and_update_map (v_1, v_2, v_3)
    | E_check_signature (v_1, v_2, v_3) ->
        Set.add (Set.add (Set.add acc v_1) v_2) v_3
    | E_create_account_address (v_1, v_2, v_3, v_4) ->
        Set.add (Set.add (Set.add (Set.add acc v_1) v_2) v_3) v_4
    | E_pair_n v_l -> Set.of_list v_l |> Set.union acc
  in
  let empty_set = Set.empty in
  function
  | Cfg_node.Cfg_assign (_, rv) -> free_variables_expr empty_set rv
  | Cfg_skip | Cfg_swap | Cfg_dig _ | Cfg_dug _ -> empty_set
  | Cfg_drop v_l -> Set.of_list v_l
  | Cfg_if_none v
  | Cfg_if_cons v
  | Cfg_loop v
  | Cfg_loop_left v
  | Cfg_map_list v
  | Cfg_map_map v
  | Cfg_iter_list v
  | Cfg_iter_set v
  | Cfg_iter_map v
  | Cfg_failwith v
  | Cfg_return v
  | Cfg_if_left v
  | Cfg_if v ->
      Set.singleton v

open Batteries

type label = int

type block = Cfg_node.t

type t = {
  nodes : (label, block) Hashtbl.t;
  initial : label;
  finals : label Set.t;
  flow : (label * label) Set.t;
  init_ht : (int, int) Hashtbl.t;
  final_ht : (int, int) Hashtbl.t;
}

let rec init ht n =
  let open Tezla.Adt in
  match n.stm with
  | S_seq (s, _) -> init ht s
  | S_assign _ | S_skip | S_map _ | S_failwith _ | S_swap | S_dig | S_dug
  | S_drop _ | S_return _ ->
      Hashtbl.find_default ht n.id n.id
  | S_iter _ | S_loop _ | S_loop_left _ -> Hashtbl.find_default ht n.id n.id
  | S_if _ | S_if_cons _ | S_if_left _ | S_if_none _ ->
      Hashtbl.find_default ht n.id n.id

let final ht =
  let open Tezla.Adt in
  let rec final_rec acc n =
    match n.stm with
    | S_if (_, x, y)
    | S_if_cons (_, x, y)
    | S_if_left (_, x, y)
    | S_if_none (_, x, y) ->
        final_rec (final_rec acc x) y
    | S_seq (_, x) -> final_rec acc x
    | S_assign _ | S_skip | S_failwith _ | S_swap | S_dig | S_dug | S_drop _
    | S_loop _ | S_loop_left _ | S_iter _ | S_map _ | S_return _ ->
        Set.add (Hashtbl.find_default ht n.id n.id) acc
  in
  final_rec Set.empty

let flow s =
  let rev_pair x y = (y, x) in
  let nodes_ht = Hashtbl.create 10 in
  let init_ht = Hashtbl.create 10 in
  let final_ht = Hashtbl.create 10 in
  let open Tezla.Adt in
  let rec flow_rec flow s =
    let new_node s =
      let n = Cfg_node.create_node s in
      let () = Hashtbl.add nodes_ht n.id n in
      n.id
    in
    let if_p cfg_s x y =
      let id = new_node cfg_s in
      let flow = flow_rec (flow_rec flow x) y in
      let init_x = init init_ht x in
      let init_y = init init_ht y in
      let flow = Set.add (id, init_x) flow |> Set.add (id, init_y) in
      flow
    in
    let loop_p n_s c v_1 v_2 b =
      let phi_id = new_node (Cfg_assign (c, E_phi (v_1, v_2))) in
      let c_node_id = new_node n_s in
      let () = Hashtbl.add init_ht s.id phi_id in
      let () = Hashtbl.add final_ht s.id c_node_id in
      let flow = flow_rec flow b in
      let flow =
        Set.map (rev_pair phi_id) (final final_ht b)
        |> Set.add (c_node_id, init init_ht b)
        |> Set.add (phi_id, c_node_id)
        |> Set.union flow
      in
      flow
    in
    let open Cfg_node in
    match s.stm with
    | S_if (c, x, y) -> if_p (Cfg_if c) x y
    | S_if_cons (c, x, y) -> if_p (Cfg_if_cons c) x y
    | S_if_left (c, x, y) -> if_p (Cfg_if_left c) x y
    | S_if_none (c, x, y) -> if_p (Cfg_if_none c) x y
    | S_loop (c, (v_1, v_2), b) -> loop_p (Cfg_loop c) c v_1 v_2 b
    | S_loop_left (c, (v_1, v_2), b) -> loop_p (Cfg_loop_left c) c v_1 v_2 b
    | S_iter (c, (v_1, v_2), b) -> loop_p (Cfg_iter c) c v_1 v_2 b
    | S_map ((c, (c_1, c_2)), (r, (r_1, r_2)), b) ->
        let phi_1_id = new_node (Cfg_assign (c, E_phi (c_1, c_2))) in
        let phi_2_id = new_node (Cfg_assign (r, E_phi (r_1, r_2))) in
        let c_node_id = new_node (Cfg_map c) in
        let () = Hashtbl.add init_ht s.id phi_1_id in
        let () = Hashtbl.add final_ht s.id c_node_id in
        let flow = flow_rec flow b in
        let flow =
          Set.map (rev_pair phi_1_id) (final final_ht b)
          |> Set.add (c_node_id, init init_ht b)
          |> Set.add (phi_2_id, c_node_id)
          |> Set.union flow
        in
        flow
    | S_seq (s_1, s_2) ->
        let flow = flow_rec (flow_rec flow s_1) s_2 in
        let init_s2 = init init_ht s_2 in
        let final_s1 = final final_ht s_1 in
        let flow = Set.map (rev_pair init_s2) final_s1 |> Set.union flow in
        flow
    | S_skip ->
        let n = new_node Cfg_skip in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
    | S_swap ->
        let n = new_node Cfg_swap in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
    | S_dig ->
        let n = new_node Cfg_dig in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
    | S_dug ->
        let n = new_node Cfg_dug in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
    | S_assign (v, e) ->
        let n = new_node (Cfg_assign (v, e)) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        let flow = flow_lambda flow e in
        flow
    | S_drop v ->
        let n = new_node (Cfg_drop v) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
    | S_failwith e ->
        let n = new_node (Cfg_failwith e) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
    | S_return e ->
        let n = new_node (Cfg_return e) in
        let () = Hashtbl.add init_ht s.id n in
        let () = Hashtbl.add final_ht s.id n in
        flow
  and flow_lambda flow =
    let open Tezla.Adt in
    function
    | E_lambda (_, _, _, s) -> flow_rec flow s
    | E_push (d, _) -> flow_data flow d
    | E_unit | E_self | E_now | E_amount | E_balance | E_source | E_sender
    | E_chain_id | E_car _ | E_cdr _ | E_abs _ | E_neg _ | E_not _
    | E_add (_, _)
    | E_sub (_, _)
    | E_mul (_, _)
    | E_div (_, _)
    | E_shiftL (_, _)
    | E_shiftR (_, _)
    | E_and (_, _)
    | E_or (_, _)
    | E_xor (_, _)
    | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _
    | E_compare (_, _)
    | E_cons (_, _)
    | E_operation _
    | E_pair (_, _)
    | E_left (_, _)
    | E_right (_, _)
    | E_some _ | E_none _
    | E_mem (_, _)
    | E_get (_, _)
    | E_update (_, _, _)
    | E_concat (_, _)
    | E_concat_list _
    | E_slice (_, _, _)
    | E_pack _
    | E_unpack (_, _)
    | E_contract_of_address (_, _)
    | E_implicit_account _
    | E_check_signature (_, _, _)
    | E_blake2b _ | E_sha256 _ | E_sha512 _ | E_hash_key _
    | E_address_of_contract _
    | E_create_contract_address (_, _, _, _)
    | E_unlift_option _ | E_unlift_or_left _ | E_unlift_or_right _ | E_hd _
    | E_tl _ | E_size _ | E_isnat _ | E_int_of_nat _
    | E_exec (_, _)
    | E_dup _ | E_nil _ | E_empty_set _
    | E_empty_map (_, _)
    | E_empty_big_map (_, _)
    | E_apply (_, _)
    | E_append (_, _)
    | E_special_empty_list _
    | E_special_empty_map (_, _)
    | E_phi (_, _) ->
        flow
  and flow_data flow =
    let open Tezla.Adt in
    function
    | D_instruction s -> flow_rec flow s
    | D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _ -> flow
    | D_left d | D_right d | D_some d -> flow_data flow d
    | D_elt (d_1, d_2) | D_pair (d_1, d_2) -> flow_data (flow_data flow d_1) d_2
    | D_list d_l -> List.fold_left flow_data flow d_l
  in

  let flow = flow_rec Set.empty s in
  let initial = init init_ht s in
  let finals = final final_ht s in
  let nodes = nodes_ht in
  { initial; finals; nodes; flow; init_ht; final_ht }

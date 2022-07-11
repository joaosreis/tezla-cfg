open! Core
open Tezla.Adt

type node = Cfg_node.t [@@deriving ord, sexp]
type edge = Edge.t * node * node [@@deriving ord, sexp]

module Flow_edge = struct
  module T = struct
    type t = edge [@@deriving ord, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Stmt = struct
  type t = Tezla.Adt.stmt [@@deriving sexp]

  let compare a b = Int.compare a.Common_adt.Node.id b.Common_adt.Node.id
  let hash s = Int.hash s.Tezla.Adt.Node.id
end

type t = {
  initial : node;
  finals : Set.M(Cfg_node).t;
  flow : Set.M(Flow_edge).t;
}

let rec init ht n =
  match n.Tezla.Adt.Node.value with
  | S_seq (s, _) -> init ht s
  | S_assign _ | S_skip | S_map_list _ | S_map_map _ | S_failwith _ | S_swap
  | S_dig _ | S_dug _ | S_drop _ | S_return _ | S_iter_list _ | S_iter_set _
  | S_iter_map _ | S_loop _ | S_loop_left _ | S_if _ | S_if_cons _ | S_if_left _
  | S_if_none _ -> (
      match Hashtbl.find ht n with
      | None ->
          Debug.amf [%here] "initial not found";
          raise (Not_found_s (Stmt.sexp_of_t n))
      | Some x -> x)

let final ht =
  let open Tezla.Adt in
  let rec final_rec acc n =
    match n.Node.value with
    | S_if (_, x, y)
    | S_if_cons (_, x, y)
    | S_if_left (_, x, y)
    | S_if_none (_, x, y) ->
        final_rec (final_rec acc x) y
    | S_seq (_, x) -> final_rec acc x
    | S_assign _ | S_skip | S_failwith _ | S_swap | S_dig _ | S_dug _ | S_drop _
    | S_loop _ | S_loop_left _ | S_iter_list _ | S_iter_set _ | S_iter_map _
    | S_map_list _ | S_map_map _ | S_return _ ->
        let f =
          match Hashtbl.find ht n with
          | None ->
              Debug.amf [%here] "final not found for %d" n.id;
              raise (Not_found_s (Int.sexp_of_t n.id))
          | Some x -> x
        in
        Set.add acc f
  in
  final_rec (Set.empty (module Cfg_node))

let flow s =
  let init_ht = Hashtbl.create (module Stmt) in
  let final_ht = Hashtbl.create (module Stmt) in
  let open Tezla.Adt in
  let rec flow_rec flow s =
    let new_node = Cfg_node.create_node in
    let if_p cfg_s x y =
      let label = new_node cfg_s in
      let () = Hashtbl.set init_ht ~key:s ~data:label in
      let () = Hashtbl.set final_ht ~key:s ~data:label in
      let flow = flow_rec (flow_rec flow x) y in
      let init_x = init init_ht x in
      let init_y = init init_ht y in
      let flow = Set.add flow (Edge.True, label, init_x) in
      let flow = Set.add flow (Edge.False, label, init_y) in
      flow
    in
    let loop_p n_s b =
      let c_node = new_node n_s in
      let () = Hashtbl.set init_ht ~key:s ~data:c_node in
      let () = Hashtbl.set final_ht ~key:s ~data:c_node in
      let flow = flow_rec flow b in
      let flow =
        let s =
          Set.map
            (module Flow_edge)
            ~f:(fun l -> (Normal, l, c_node))
            (final final_ht b)
        in
        let s = Set.add s (True, c_node, init init_ht b) in
        Set.union flow s
      in
      flow
    in
    match s.value with
    | S_if (c, x, y) -> if_p (Cfg_if c) x y
    | S_if_cons (c, x, y) -> if_p (Cfg_if_cons c) x y
    | S_if_left (c, x, y) -> if_p (Cfg_if_left c) x y
    | S_if_none (c, x, y) -> if_p (Cfg_if_none c) x y
    | S_loop (c, b) -> loop_p (Cfg_loop c) b
    | S_loop_left (c, b) -> loop_p (Cfg_loop_left c) b
    | (S_iter_list (c, b) | S_iter_set (c, b) | S_iter_map (c, b)) as i ->
        loop_p
          (match i with
          | S_iter_list (_, _) -> Cfg_iter_list c
          | S_iter_set (_, _) -> Cfg_iter_set c
          | S_iter_map (_, _) -> Cfg_iter_map c
          | _ -> assert false)
          b
    | (S_map_list (c, b) | S_map_map (c, b)) as i ->
        let c_node =
          new_node
            (match i with
            | S_map_list _ -> Cfg_map_list c
            | S_map_map _ -> Cfg_map_map c
            | _ -> assert false)
        in
        let () = Hashtbl.set init_ht ~key:s ~data:c_node in
        let () = Hashtbl.set final_ht ~key:s ~data:c_node in
        let flow = flow_rec flow b in
        let flow =
          let s =
            Set.fold (final final_ht b)
              ~f:(fun acc x -> Set.add acc (Normal, x, c_node))
              ~init:(Set.empty (module Flow_edge))
          in
          let s = Set.add s (True, c_node, init init_ht b) in
          Set.union flow s
        in
        flow
    | S_seq (s_1, s_2) ->
        let flow = flow_rec (flow_rec flow s_1) s_2 in
        let init_s2 = init init_ht s_2 in
        let final_s1 = final final_ht s_1 in
        let flow =
          Set.map (module Flow_edge) ~f:(fun l -> (Normal, l, init_s2)) final_s1
          |> Set.union flow
        in
        flow
    | S_skip ->
        let n = new_node Cfg_skip in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
    | S_swap ->
        let n = new_node Cfg_swap in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
    | S_dig n ->
        let n = new_node (Cfg_dig n) in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
    | S_dug n ->
        let n = new_node (Cfg_dug n) in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
    | S_assign (v, e) ->
        let n = new_node (Cfg_assign (v, e)) in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        let flow = flow_lambda flow e in
        flow
    | S_drop v ->
        let n = new_node (Cfg_drop v) in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
    | S_failwith e ->
        let n = new_node (Cfg_failwith e) in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
    | S_return e ->
        let n = new_node (Cfg_return e) in
        let () = Hashtbl.set init_ht ~key:s ~data:n in
        let () = Hashtbl.set final_ht ~key:s ~data:n in
        flow
  and flow_lambda flow e =
    let open Tezla.Adt in
    match e.value with
    | E_lambda (_, _, s) -> flow_rec flow s
    | E_push d -> flow_data flow d
    | E_unit | E_self | E_now | E_amount | E_balance | E_source | E_sender
    | E_chain_id | E_car _ | E_cdr _ | E_abs _ | E_neg_bls12_381_fr _
    | E_neg_bls12_381_g1 _ | E_neg_bls12_381_g2 _ | E_neg_int _ | E_neg_nat _
    | E_not_bool _ | E_not_nat _ | E_not_int _
    | E_add_bls12_381_fr (_, _)
    | E_add_bls12_381_g1 (_, _)
    | E_add_bls12_381_g2 (_, _)
    | E_add_int (_, _)
    | E_add_mutez (_, _)
    | E_add_nat (_, _)
    | E_add_nat_int (_, _)
    | E_add_timestamp_int (_, _)
    | E_sub_int (_, _)
    | E_sub_nat (_, _)
    | E_sub_nat_int (_, _)
    | E_sub_mutez (_, _)
    | E_sub_timestamp (_, _)
    | E_sub_timestamp_int (_, _)
    | E_mul_bls12_381_fr_bls12_381_fr (_, _)
    | E_mul_bls12_381_g1_bls12_381_fr (_, _)
    | E_mul_bls12_381_g2_bls12_381_fr (_, _)
    | E_mul_int (_, _)
    | E_mul_nat (_, _)
    | E_mul_nat_int (_, _)
    | E_mul_mutez_nat (_, _)
    | E_mul_int_bls12_381_fr (_, _)
    | E_mul_nat_bls12_381_fr (_, _)
    | E_ediv_int (_, _)
    | E_ediv_nat (_, _)
    | E_ediv_nat_int (_, _)
    | E_ediv_mutez (_, _)
    | E_ediv_mutez_nat (_, _)
    | E_lsl (_, _)
    | E_lsr (_, _)
    | E_and_bool (_, _)
    | E_and_int_nat (_, _)
    | E_and_nat (_, _)
    | E_or_bool (_, _)
    | E_or_nat (_, _)
    | E_xor_bool (_, _)
    | E_xor_nat (_, _)
    | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _
    | E_compare (_, _)
    | E_cons (_, _)
    | E_operation _
    | E_pair (_, _)
    | E_left (_, _)
    | E_right (_, _)
    | E_some _ | E_none _
    | E_mem_set (_, _)
    | E_mem_map (_, _)
    | E_mem_big_map (_, _)
    | E_get_map (_, _)
    | E_get_big_map (_, _)
    | E_update_set (_, _, _)
    | E_update_map (_, _, _)
    | E_update_big_map (_, _, _)
    | E_concat_bytes (_, _)
    | E_concat_string (_, _)
    | E_concat_list_string _ | E_concat_list_bytes _
    | E_slice_bytes (_, _, _)
    | E_slice_string (_, _, _)
    | E_pack _
    | E_unpack (_, _)
    | E_contract_of_address (_, _)
    | E_implicit_account _
    | E_check_signature (_, _, _)
    | E_blake2b _ | E_sha256 _ | E_sha512 _ | E_hash_key _
    | E_address_of_contract _
    | E_create_contract_address (_, _, _, _)
    | E_unlift_option _ | E_unlift_or_left _ | E_unlift_or_right _ | E_hd _
    | E_tl _ | E_isnat _ | E_int_of_nat _
    | E_exec (_, _)
    | E_dup _ | E_nil _ | E_empty_set _
    | E_empty_map (_, _)
    | E_empty_big_map (_, _)
    | E_apply (_, _)
    | E_list_append (_, _)
    | E_special_empty_list _
    | E_special_empty_map (_, _)
    | E_total_voting_power | E_self_address | E_level | E_size_bytes _
    | E_size_string _ | E_size_list _ | E_size_set _ | E_size_map _
    | E_create_account_address (_, _, _, _)
    | E_voting_power _ | E_keccak _ | E_sha3 _ | E_pairing_check _
    | E_sapling_verify_update (_, _)
    | E_sapling_empty_state _
    | E_ticket (_, _)
    | E_read_ticket_pair _ | E_read_ticket_ticket _
    | E_split_ticket (_, _)
    | E_join_ticket _
    | E_open_chest (_, _, _)
    | E_get_and_update_val (_, _, _)
    | E_get_and_update_map (_, _, _)
    | E_dup_n (_, _)
    | E_get_n (_, _)
    | E_update_n (_, _, _)
    | E_var _ | E_pair_n _ ->
        flow
  and flow_data flow d =
    let open Tezla.Adt in
    match snd d.value with
    | D_instruction (_, s) -> flow_rec flow s
    | D_unit | D_none | D_int _ | D_string _ | D_bytes _ | D_bool _ -> flow
    | D_left d | D_right d | D_some d -> flow_data flow d
    | D_pair (d_1, d_2) -> flow_data (flow_data flow d_1) d_2
    | D_list d_l -> List.fold_left ~f:flow_data ~init:flow d_l
    | D_map d_l ->
        List.fold_left
          ~f:(fun acc (d_1, d_2) -> flow_data (flow_data acc d_1) d_2)
          ~init:flow d_l
  in
  let flow = flow_rec (Set.empty (module Flow_edge)) s in
  let initial = init init_ht s in
  let finals = final final_ht s in
  { initial; finals; flow }

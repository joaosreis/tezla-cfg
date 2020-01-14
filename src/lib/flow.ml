open Batteries

type vertex = Cfg_node.t

type t = {
  nodes : vertex Set.t;
  initial : vertex Set.t;
  final : vertex Set.t;
  flow : (vertex * vertex) Set.t;
}

let rec init n =
  let open Tezla.Adt in
  match n.stm with
  | S_seq (s, _) -> init s
  | S_var_decl _ | S_decl_assign _ | S_assign _ | S_skip | S_map _ | S_cast
  | S_failwith _ | S_contract _ | S_swap | S_dig | S_dug | S_drop _ ->
      n.id
  | S_iter _ | S_loop _ | S_loop_left _ -> n.id
  | S_if _ | S_if_cons _ | S_if_left _ | S_if_none _ -> n.id

let final x =
  let open Set.Infix in
  let open Tezla.Adt in
  let rec final_rec acc n =
    match n.stm with
    | S_if (_, x, y)
    | S_if_cons (_, x, _, _, y)
    | S_if_left (_, x, y, _)
    | S_if_none (_, x, y, _) ->
        final_rec (final_rec acc x) y
    | S_seq (_, x) -> final_rec acc x
    | S_var_decl _ | S_decl_assign _ | S_assign _ | S_skip | S_cast
    | S_failwith _ | S_contract _ | S_swap | S_dig | S_dug | S_drop _
    | S_loop _ | S_loop_left _ | S_iter _ | S_map _ ->
        acc <-- n.id
  in
  final_rec Set.empty x

let flow x =
  let open Tezla.Adt in
  let open Set.Infix in
  let rev_pair x y = (y, x) in
  let ht = Hashtbl.create 10 in
  let rec flow_rec (nodes, flow) n =
    let new_node s =
      let n' = Cfg_node.create_node s in
      Hashtbl.add ht n.id n'
    in
    let if_p x y =
      let nodes' = nodes <-- n.id in
      let flow' = flow <-- (n.id, init x) <-- (n.id, init y) in
      flow_rec (flow_rec (nodes', flow') x) y
    in
    let loop_p b =
      let nodes' = nodes <-- n.id in
      let flow' =
        Set.map (rev_pair n.id) (final b) <-- (n.id, init b) ||. flow
      in
      flow_rec (nodes', flow') b
    in
    let open Cfg_node in
    match n.stm with
    | S_if (c, x, y) ->
        let () = new_node (Cfg_if c) in
        if_p x y
    | S_if_cons (c, x, u, v, y) ->
        let () = new_node (Cfg_if_cons (c, u, v)) in
        if_p x y
    | S_if_left (c, x, y, u) ->
        let () = new_node (Cfg_if_left (c, u)) in
        if_p x y
    | S_if_none (c, x, y, u) ->
        let () = new_node (Cfg_if_none (c, u)) in
        if_p x y
    | S_loop (c, b) ->
        let () = new_node (Cfg_loop c) in
        loop_p b
    | S_loop_left (c, b) ->
        let () = new_node (Cfg_loop_left c) in
        loop_p b
    | S_iter (c, b) ->
        let () = new_node (Cfg_iter c) in
        loop_p b
    | S_map (c, b) ->
        let () = new_node (Cfg_map c) in
        loop_p b
    | S_seq (s_1, s_2) ->
        let init_s2 = init s_2 in
        let final_s1 = final s_1 in
        let flow' = flow ||. Set.map (rev_pair init_s2) final_s1 in
        flow_rec (flow_rec (nodes, flow') s_1) s_2
    | S_skip ->
        let () = new_node Cfg_skip in
        (nodes <-- n.id, flow)
    | S_swap ->
        let () = new_node Cfg_swap in
        (nodes <-- n.id, flow)
    | S_dig ->
        let () = new_node Cfg_dig in
        (nodes <-- n.id, flow)
    | S_dug ->
        let () = new_node Cfg_dug in
        (nodes <-- n.id, flow)
    | S_cast ->
        let () = new_node Cfg_cast in
        (nodes <-- n.id, flow)
    | S_var_decl (v, t) ->
        let () = new_node (Cfg_var_decl (v, t)) in
        (nodes <-- n.id, flow)
    | S_decl_assign (v, e, t) ->
        let () = new_node (Cfg_decl_assign (v, e, t)) in
        (nodes <-- n.id, flow)
    | S_assign (v, e) ->
        let () = new_node (Cfg_assign (v, e)) in
        (nodes <-- n.id, flow)
    | S_drop v ->
        let () = new_node (Cfg_drop v) in
        (nodes <-- n.id, flow)
    | S_failwith e ->
        let () = new_node (Cfg_failwith e) in
        (nodes <-- n.id, flow)
    | S_contract t ->
        let () = new_node (Cfg_contract t) in
        (nodes <-- n.id, flow)
  in

  let nodes', flow = flow_rec (Set.empty, Set.empty) x in
  let initial = Hashtbl.find ht (init x) |> Set.singleton in
  let final = Set.map (Hashtbl.find ht) (final x) in
  let nodes = Set.map (Hashtbl.find ht) nodes' in
  let flow =
    Set.map (fun (a, b) -> (Hashtbl.find ht a, Hashtbl.find ht b)) flow
  in
  { initial; final; nodes; flow }

let flowR n =
  let f = flow n in
  let flow = Set.map (fun (a, b) -> (b, a)) f.flow in
  { f with initial = f.final; final = f.initial; flow }

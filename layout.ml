type 'a node_data = {
  v: 'a;
  x: float ref;
  y: float ref;
  mod_val: float ref;
  l: 'a tree option;
  r: 'a tree option
}
and 'a tree =
  | Empty
  | Node of 'a node_data

(* Set sibling distance to 1 *)
let sibling_distance = 1.0

(* Helper method to apply a shift on a node *)
let rec apply_shift node shift =
  match node with
  | Empty -> ()
  | Node {x; mod_val; l; r; _} ->
      x := !x +. shift;
      (* mod_val := !mod_val +. shift; *)
      (match l with Some left -> apply_shift left shift | None -> ());
      (match r with Some right -> apply_shift right shift | None -> ())

let rec initial_pos node depth is_right_child =
  match node with
  | Empty -> ()
  | Node {v; x; y; mod_val; l; r} ->  (* removed 'as n' *)
      y := float_of_int depth;
      if is_right_child then x := 1.0 else x := 0.0;
      
      (match l with
      | Some left -> initial_pos left (depth + 1) false
      | None -> ());
     
      (match r with
      | Some right -> initial_pos right (depth + 1) true
      | None -> ());

      match (l, r) with
      | (None, None) | (Some Empty, _) | (_, Some Empty) ->
          mod_val := 0.0
      | (Some (Node left_node), None) ->
          left_node.x := !x
      | (None, Some (Node right_node)) ->
          right_node.x := !x
      | (Some (Node left_node), Some (Node right_node)) ->
          let mid_point = (!(left_node.x) +. !(right_node.x)) /. 2.0 in
          x := mid_point;
          let right_shift = !x +. sibling_distance -. !(right_node.x) in
          apply_shift (Node right_node) right_shift

let second_pass tree =
  (* Track if smallest x is negative *)
  let min_x = ref 0.0 in
  let rec process_node node acc_mod =
    match node with
    | Empty -> ()
    | Node {x; mod_val; l; r; _} ->
        let final_x = !x +. acc_mod in
        x := final_x;
        
        if final_x < !min_x then
          min_x := final_x; 
        
        (* Stack mod val *)
        let new_acc_mod = acc_mod +. !mod_val in
        
        (* Recursive call *)
        (match l with 
          | Some left -> process_node left new_acc_mod 
          | None -> ());
        (match r with 
          | Some right -> process_node right new_acc_mod 
          | None -> ())
  in
  process_node tree 0.0;
  !min_x

(* Fix negative value if necessary *)
let normalize_coordinates tree min_x =
  if min_x < 0.0 then
    apply_shift tree (abs_float min_x)

(* Main Call *)
let main tree =
  let min_x = second_pass tree in
  normalize_coordinates tree min_x
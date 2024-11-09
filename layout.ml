(* Definition of the tree's type, assuming it's a binary tree *)
type 'a tree = 
  | Empty 
  | Node of { 
      v: 'a; 
      x: float ref; 
      y: float ref; 
      mod_val: float ref; 
      l: 'a tree option; 
      r: 'a tree option 
    }

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
  | Node {v; x; y; mod_val; l; r} as n ->

      y := float_of_int depth;

      if is_right_child then x := 1.0 else x := 0.0;

      (match l with 
      | Some left -> initial_pos left (depth + 1) false
      | None -> ());
      
      (match r with 
      | Some right -> initial_pos right (depth + 1) true
      | None -> ());

      match (l, r) with
      | (None, None) ->
          mod_val := 0.0

      | (Some left_child, None) ->
          left_child.x := !x

      | (None, Some right_child) ->
          right_child.x := !x

      | (Some {x = lx; _} as left_child, Some {x = rx; _} as right_child) ->
          let mid_point = (!lx +. !rx) /. 2.0 in
          x := mid_point;

          let right_shift = !x +. sibling_distance -. !rx in

          (* Apply shift to the entire right subtree *)
          apply_shift (Some right_child) right_shift;

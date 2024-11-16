(* By Kevin Liu (261136372), David Zhou(261135446), and Yessine Chaari (261179816) *)

(********************************************************************
******************** Types and helper functions *********************
********************************************************************)
exception NotImplemented

(* Type for tree nodes *)
type 'a node_data = {
  v: 'a;
  x: float ref;
  y: float ref;
  mod_val: float ref;
  shift_val: float ref;
  xf: float ref; 
  l: 'a tree option;
  r: 'a tree option;
}
and 'a tree =
  | Empty
  | Node of 'a node_data

(* Retrieves the x coordinate of a node *)
let get_x (tree: 'a tree) : float = 
  match tree with
  | Empty -> 0.0
  | Node n -> !(n.x)

(* Get the maximum depth of a tree *)
let rec get_max_depth (tree: 'a tree) : int =
  match tree with
  | Empty -> 0
  | Node n ->
      let left_depth = match n.l with
        | Some left -> get_max_depth left
        | None -> 0 in
      let right_depth = match n.r with
        | Some right -> get_max_depth right
        | None -> 0 in
      1 + max left_depth right_depth

(* Get the rightmost descendant at a specific depth *)
let rec get_rightmost_descendant (tree: 'a tree) (current_depth: int) (target_depth: int) : 'a tree option =
  match tree with
  | Empty -> None
  | Node n when current_depth = target_depth -> Some (Node n)
  | Node n -> 
      let right_result = match n.r with
        | Some right -> get_rightmost_descendant right (current_depth + 1) target_depth
        | None -> None in
      if right_result <> None then right_result
      else match n.l with
        | Some left -> get_rightmost_descendant left (current_depth + 1) target_depth
        | None -> None

(* Get the leftmost descendant at a specific depth *)
let rec get_leftmost_descendant (tree: 'a tree) (current_depth: int) (target_depth: int) : 'a tree option =
  match tree with
  | Empty -> None
  | Node n when current_depth = target_depth -> Some (Node n)
  | Node n ->
      let left_result = match n.l with
        | Some left -> get_leftmost_descendant left (current_depth + 1) target_depth
        | None -> None in
      if left_result <> None then left_result
      else match n.r with
        | Some right -> get_leftmost_descendant right (current_depth + 1) target_depth
        | None -> None

(* Calculate x coordinate using accumulated xf *)
let calculate_x_coordinate (node: 'a node_data) : float =
  !(node.x) +. !(node.xf)

(* Check subtree conflicts, using only xf for position *)
let check_subtree_conflicts (right_tree: 'a tree) (left_tree: 'a tree) (subtree_distance: float) : float =
  let max_depth = max (get_max_depth right_tree) (get_max_depth left_tree) in
  let max_shift = ref 0.0 in

  for depth = 0 to max_depth do
    match (get_rightmost_descendant left_tree 0 depth, 
           get_leftmost_descendant right_tree 0 depth) with
    | Some (Node left_contour), Some (Node right_contour) -> 
        let left_x = !(left_contour.xf) in
        let right_x = !(right_contour.xf) in

        let required_shift = left_x +. subtree_distance -. right_x in
        if required_shift > !max_shift then
          max_shift := required_shift
    | _ -> ()
  done;
  !max_shift

(********************************************************************
******************** WRITE YOUR CODE HERE****** *********************
********************************************************************)

(* Applies a shift only to the right sibling of a node *)
let rec apply_shift_to_siblings (prev: 'a node_data option) (shift: float) : unit =
  raise NotImplemented

(* First Pass *)
let rec first_pass_p1 (tree: 'a tree) (is_right: bool) (prev: 'a tree option) (depth: int) : unit =
  raise NotImplemented

let rec first_pass_p2 (tree: 'a tree) (ancestor_mods: float list) : unit =
  raise NotImplemented

let second_pass (tree: 'a tree) : unit =
  raise NotImplemented

let fix_root (tree: 'a tree) : unit =
  raise NotImplemented

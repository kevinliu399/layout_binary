(* By Kevin Liu (261136372), David Zhou(261135446), and Yessine Chaari (261179816) *)

(********************************************************************
******************** TYPES AND HELPER FUNCTIONS *********************
********************************************************************)

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

exception NotImplemented

(* Retrieves the x coordinate of a node *)
let get_x (tree: 'a tree) : float = 
  match tree with
  | Empty -> 0.0
  | Node n -> !(n.x)

(* Helpers for traversal 2*)
let shift_distance = 0.5
type contour = {
  leftmost: float;   
  rightmost: float;  
  height: int        
}
let make_empty_contour () = {
  leftmost = max_float;
  rightmost = -.max_float;
  height = 0
}
(* Merge two contours and return required shift amount *)
let merge_contours left_c right_c min_distance =
  let separation = right_c.leftmost -. left_c.rightmost in
  if separation < min_distance then
    let shift = min_distance -. separation in
    Printf.printf "Conflict detected! Required shift: %.2f\n" shift;
    shift
  else
    0.0

let get_xf tree = match tree with
| Empty -> 0.0
| Node n -> !(n.xf)

(********************************************************************
************************** MAIN ALGORITHM ***************************
********************************************************************)

let rec traversal_one (tree: 'a tree) (is_right: bool) (prev: 'a tree option) (depth: int) : unit =
  raise NotImplemented

let rec traversal_two (tree: 'a tree) (ancestor_mods: float list) (right_sibling: 'a tree option) : contour = 
  (* Refer to the top of the code for helper functions that can assist you in the implementation *)
  raise NotImplemented

let traversal_three (tree: 'a tree) : unit =
  raise NotImplemented

let fix_root (tree: 'a tree) =
  raise NotImplemented

(********************************************************************
************************** MAIN FUNCTION ***************************
********************************************************************)

let main tree =
  traversal_one tree false None 0;
  traversal_two tree [] None;
  traversal_three tree;
  fix_root tree;
  ()

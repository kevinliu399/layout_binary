(* By Kevin Liu (261136372), David Zhou(261135446), and Yessine Chaari (261179816) *)

(********************************************************************
******************** TYPES AND HELPER FUNCTIONS *********************
********************************************************************)
exception NotImplemented

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

(* Get the descendant at a specific depth *)
let rec get_descendant (tree: 'a tree) (current_depth: int) (target_depth: int)
    (first: 'a node_data -> 'a tree option) (second: 'a node_data -> 'a tree option) : 'a tree option =
  match tree with
  | Empty -> None
  | Node n when current_depth = target_depth -> Some (Node n)
  | Node n ->
      (* Try the first direction *)
      let first_child = first n in
      match first_child with
      | Some child -> 
          let first_result = get_descendant child (current_depth + 1) target_depth first second in
          if first_result <> None then first_result
          else (
            (* If first direction fails, try the second direction *)
            let second_child = second n in
            match second_child with
            | Some child -> get_descendant child (current_depth + 1) target_depth first second
            | None -> None
          )
      | None -> 
          (* If first direction doesn't exist, try the second direction *)
          let second_child = second n in
          match second_child with
          | Some child -> get_descendant child (current_depth + 1) target_depth first second
          | None -> None

(* Get rightmost descendant: tries right first, then left *)
let get_rightmost_descendant tree current_depth target_depth =
  get_descendant tree current_depth target_depth (fun n -> n.r) (fun n -> n.l)  

(* Get leftmost descendant: tries left first, then right *)
let get_leftmost_descendant tree current_depth target_depth =
  get_descendant tree current_depth target_depth (fun n -> n.l) (fun n -> n.r) 

(********************************************************************
************************** MAIN ALGORITHM ***************************
********************************************************************)

(* Applies a shift only to the right sibling of a node *)
let rec apply_shift_to_siblings (prev: 'a node_data option) (shift: float) : unit =
  raise NotImplemented

let rec traversal_one (tree: 'a tree) (is_right: bool) (prev: 'a tree option) (depth: int) : unit =
  raise NotImplemented

let rec traversal_two (tree: 'a tree) (ancestor_mods: float list) : unit =
  raise NotImplemented

let traversal_three (tree: 'a tree) : unit =
  raise NotImplemented

let fix_root (tree: 'a tree) : unit =
  raise NotImplemented

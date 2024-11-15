(* Define the node type with xf for accumulated modifications *)
type 'a node_data = {
  v: 'a;
  x: float ref;
  y: float ref;
  mod_val: float ref;
  shift_val: float ref;
  xf: float ref;  (* Accumulated field for shifts and mods *)
  l: 'a tree option;
  r: 'a tree option;
}
and 'a tree =
  | Empty
  | Node of 'a node_data

(* Helper function to get the maximum depth of a tree *)
let rec get_max_depth = function
  | Empty -> 0
  | Node n ->
      let left_depth = match n.l with
        | Some left -> get_max_depth left
        | None -> 0 in
      let right_depth = match n.r with
        | Some right -> get_max_depth right
        | None -> 0 in
      1 + max left_depth right_depth

(* Helper function to get the rightmost descendant at a specific depth *)
let rec get_rightmost_descendant tree current_depth target_depth =
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

(* Helper function to get the leftmost descendant at a specific depth *)
let rec get_leftmost_descendant tree current_depth target_depth =
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
let calculate_x_coordinate node =
  !(node.x) +. !(node.xf)

(* Check subtree conflicts, using only xf for position *)
let check_subtree_conflicts right_tree left_tree subtree_distance =
  let max_depth = max (get_max_depth right_tree) (get_max_depth left_tree) in
  let max_shift = ref 0.0 in

  for depth = 0 to max_depth do
    match (get_rightmost_descendant left_tree 0 depth, 
           get_leftmost_descendant right_tree 0 depth) with
    | Some (Node left_contour), Some (Node right_contour) ->
        (* Use only xf value since it already includes the base x position *)
        let left_x = !(left_contour.xf) in
        let right_x = !(right_contour.xf) in

        let required_shift = left_x +. subtree_distance -. right_x in
        if required_shift > !max_shift then
          max_shift := required_shift
    | _ -> ()
  done;
  !max_shift

(* First pass for accumulating xf modifications and shifts *)
let rec first_pass_part2 tree ancestor_mods ancestor_shifts =
  match tree with
  | Empty -> ()
  | Node n ->
      (* Calculate accumulated xf before processing children *)
      let current_mods = List.fold_left (+.) 0.0 ancestor_mods in
      let current_shifts = List.fold_left (+.) 0.0 ancestor_shifts in
      n.xf := !(n.x) +. current_mods +. current_shifts;

      (* Process left subtree *)
      (match n.l with 
       | Some left -> first_pass_part2 left (!(n.mod_val) :: ancestor_mods) (!(n.shift_val) :: ancestor_shifts)
       | None -> ());

      (* Process right subtree *)
      (match n.r with
       | Some right -> 
           first_pass_part2 right (!(n.mod_val) :: ancestor_mods) (!(n.shift_val) :: ancestor_shifts);

           (* Check conflicts with left subtree *)
           (match n.l with
            | Some left ->
                let shift = check_subtree_conflicts right left 1.0 in
                if shift > 0.0 then
                  (match right with
                   | Node right_data -> 
                       right_data.shift_val := shift;
                       (* Update xf for the right subtree *)
                       right_data.xf := !(right_data.xf) +. shift
                   | Empty -> ())
            | None -> ())
       | None -> ())

(* Start the first pass with an empty ancestor list for mods and shifts *)
let start_first_pass_part2 tree =
  first_pass_part2 tree [] []

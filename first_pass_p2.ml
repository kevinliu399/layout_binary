(* Helper function to get the leftmost descendant at a given depth *)
let rec get_leftmost_descendant tree current_depth target_depth =
  if current_depth = target_depth then Some tree
  else if current_depth > target_depth then None
  else 
    match tree with
    | Empty -> None
    | Node n -> 
        match n.l with
        | Some left -> get_leftmost_descendant left (current_depth + 1) target_depth
        | None -> None

(* Helper function to get the rightmost descendant at a given depth *)
let rec get_rightmost_descendant tree current_depth target_depth =
  if current_depth = target_depth then Some tree
  else if current_depth > target_depth then None
  else 
    match tree with
    | Empty -> None
    | Node n -> 
        match n.r with
        | Some right -> get_rightmost_descendant right (current_depth + 1) target_depth
        | None -> None

(* Helper function to get the maximum depth of a subtree *)
let rec get_max_depth = function
  | Empty -> 0
  | Node n -> 
      let left_depth = match n.l with 
        | Some l -> get_max_depth l 
        | None -> 0 in
      let right_depth = match n.r with 
        | Some r -> get_max_depth r 
        | None -> 0 in
      1 + max left_depth right_depth

(* Helper function to calculate actual x coordinate with modifications *)
let calculate_x_coordinate node ancestor_mods =
  !(node.x) +. List.fold_left (+.) 0.0 ancestor_mods

(* Function to check subtree conflicts between a right subtree and a left subtree *)
let check_subtree_conflicts right_tree left_tree subtree_distance ancestor_mods =
  let max_depth = max (get_max_depth right_tree) (get_max_depth left_tree) in
  let max_shift = ref 0.0 in
  
  (* Check conflicts at each depth level *)
  for depth = 0 to max_depth do
    match (get_rightmost_descendant left_tree 0 depth, 
           get_leftmost_descendant right_tree 0 depth) with
    | Some (Node left_contour), Some (Node right_contour) ->
        (* Calculate actual coordinates considering all modifications *)
        let left_x = calculate_x_coordinate left_contour ancestor_mods in
        let right_x = calculate_x_coordinate right_contour ancestor_mods in
        
        (* Calculate required shift to maintain minimum distance *)
        let required_shift = left_x +. subtree_distance -. right_x in
        if required_shift > !max_shift then
          max_shift := required_shift
    | _ -> ()
  done;
  !max_shift

(* Main function for Part 2 of First Pass *)
let rec first_pass_part2 tree ancestor_mods =
  match tree with
  | Empty -> ()
  | Node n ->
      (* Process left subtree *)
      (match n.l with 
       | Some left -> first_pass_part2 left (!(n.mod_val) :: ancestor_mods)
       | None -> ());
      
      (* Process right subtree *)
      (match n.r with
       | Some right -> 
           (* Process the right subtree first *)
           first_pass_part2 right (!(n.mod_val) :: ancestor_mods);
           
           (* Check for conflicts with the left subtree *)
           (match n.l with
            | Some left ->
                let shift = check_subtree_conflicts right left sibling_distance ancestor_mods in
                if shift > 0.0 then
                  n.mod_val := !(n.mod_val) +. shift
            | None -> ())
       | None -> ())

(* Main entry point that initiates the second part of first pass *)
let start_first_pass_part2 tree =
  first_pass_part2 tree []

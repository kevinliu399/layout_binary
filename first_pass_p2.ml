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

(* Helper function to get maximum depth of a tree *)
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

(* Check subtree conflicts, taking xf into account *)
let check_subtree_conflicts right_tree left_tree subtree_distance =
  let max_depth = max (get_max_depth right_tree) (get_max_depth left_tree) in
  let max_shift = ref 0.0 in

  for depth = 0 to max_depth do
    match (get_rightmost_descendant left_tree 0 depth, 
           get_leftmost_descendant right_tree 0 depth) with
    | Some (Node left_contour), Some (Node right_contour) ->
        let left_x = calculate_x_coordinate left_contour in
        let right_x = calculate_x_coordinate right_contour in

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
      (* Accumulate all shifts and mods from ancestors *)
      n.xf := List.fold_left (+.) 0.0 ancestor_mods +. List.fold_left (+.) 0.0 ancestor_shifts;

      (* Recur on the left subtree *)
      (match n.l with 
       | Some left -> first_pass_part2 left (!(n.mod_val) :: ancestor_mods) (!(n.shift_val) :: ancestor_shifts)
       | None -> ());

      (* Recur on the right subtree *)
      (match n.r with
       | Some right -> 
           first_pass_part2 right (!(n.mod_val) :: ancestor_mods) (!(n.shift_val) :: ancestor_shifts);

           (* Check for conflicts between left and right subtrees *)
           (match n.l with
            | Some left ->
                let shift = check_subtree_conflicts right left 1.0 in
                if shift > 0.0 then
                  (match right with
                   | Node right_data -> 
                       right_data.shift_val := !(right_data.shift_val) +. shift
                   | Empty -> ())
            | None -> ())
       | None -> ())

(* Start the first pass with an empty ancestor list for mods and shifts *)
let start_first_pass_part2 tree =
  first_pass_part2 tree [] []

(* Example tree structure *)
let create_example_tree () =
  let a = { v = "A"; x = ref 0.0; y = ref 0.0; mod_val = ref 0.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in
  let b = { v = "B"; x = ref 0.0; y = ref 0.0; mod_val = ref 2.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in
  let c = { v = "C"; x = ref 0.0; y = ref 0.0; mod_val = ref 0.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in
  let d = { v = "D"; x = ref 0.0; y = ref 0.0; mod_val = ref 0.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in
  let e = { v = "E"; x = ref 3.0; y = ref 0.0; mod_val = ref 0.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in
  let f = { v = "F"; x = ref 0.0; y = ref 0.0; mod_val = ref 0.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in
  let g = { v = "G"; x = ref 0.0; y = ref 0.0; mod_val = ref 0.0; shift_val = ref 0.0; xf = ref 0.0; l = None; r = None } in

  (* Setting up the tree hierarchy *)
  a.l <- Some (Node b);
  a.r <- Some (Node c);
  b.l <- Some (Node d);
  b.r <- Some (Node e);
  c.l <- Some (Node f);
  c.r <- Some (Node g);

  Node a

(* Print function for debugging *)
let print_tree tree =
  let rec aux depth = function
    | Empty -> ()
    | Node n ->
        Printf.printf "Node %s: (x=%.1f, y=%.1f, mod=%.1f, shift=%.1f, xf=%.1f)\n"
          n.v !(n.x) !(n.y) !(n.mod_val) !(n.shift_val) !(n.xf);
        (match n.l with Some left -> aux (depth + 1) left | None -> ());
        (match n.r with Some right -> aux (depth + 1) right | None -> ())
  in aux 0 tree

(* Run the first pass and print the results *)
let () =
  let tree = create_example_tree () in
  Printf.printf "Before first pass:\n";
  print_tree tree;
  start_first_pass_part2 tree;
  Printf.printf "\nAfter all passes:\n";
  print_tree tree


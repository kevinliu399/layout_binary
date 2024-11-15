(********************************************************************
******************** Types and helper functions *********************
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

(* Apply a horizontal shift to a node and its children *)
let rec apply_shift node shift =
  match node with
  | Empty -> ()
  | Node {x; mod_val; l; r; _} ->
      x := !x +. shift;
      (match l with Some left -> apply_shift left shift | None -> ());
      (match r with Some right -> apply_shift right shift | None -> ())

(* Applies a shift only to the right sibling of a node *)
let rec apply_shift_to_siblings prev shift =
  match prev with
  | None -> ()
  | Some parent ->
      match parent.r with
      | Some right -> apply_shift right shift
      | None -> ()

(* Retrieves the x coordinate of a node *)
let get_x tree = match tree with
  | Empty -> 0.0
  | Node n -> !(n.x)

exception NotImplemented

(********************************************************************
******************** WRITE YOUR CODE BELOW **************************
********************************************************************)

(* First Pass *)
let rec first_pass_p1 tree is_right prev depth =
  match tree with
  | Empty -> ()
  | Node n ->
      (* Initially position nodes *)
      n.x := (if is_right then !(n.x) +. 1.0 else 0.0);
      n.y := float_of_int depth;
      
      (* Process children *)
      (match n.l with 
       | Some l -> first_pass_p1 l false (Some tree) (depth + 1)
       | None -> ());
      (match n.r with
       | Some r -> first_pass_p1 r true (Some tree) (depth + 1)
       | None -> ());
      
      (* Calculate midpoint of children *)
      let children_midpoint = match (n.l, n.r) with
        | (Some l, Some r) -> (get_x l +. get_x r) /. 2.0
        | (Some l, None) -> get_x l
        | (None, Some r) -> get_x r
        | (None, None) -> !(n.x)
      in

      (* For left nodes with children, shift based on children's midpoint *)
      if not is_right && (n.l != None || n.r != None) then
        let original_x = !(n.x) in
        n.x := children_midpoint;
        let shift_amount = children_midpoint -. original_x in
        
        (* Apply the same shift to the right sibling *)
        match prev with
        | Some (Node parent) -> 
            (match parent.r with
             | Some (Node right_sibling) ->
                 let new_right_x = !(right_sibling.x) +. shift_amount in
                 right_sibling.x := new_right_x;
                 (* Debug print
                 Printf.printf "Shifting right sibling %s from %.1f to %.1f (shift: %.1f)\n" 
                   right_sibling.v !(right_sibling.x) new_right_x shift_amount; *)
             | Some Empty | None -> ())
        | None | Some Empty -> ()
      else if is_right && (n.l != None || n.r != None) then
        (* Case 2: Non-leftmost node with children - set mod *)
        n.mod_val := !(n.x) -. children_midpoint

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

(* First pass for accumulating xf modifications without shifts *)
let rec first_pass_part2 tree ancestor_mods =
  match tree with
  | Empty -> ()
  | Node n ->
      (* Calculate accumulated xf before processing children *)
      let current_mods = List.fold_left (+.) 0.0 ancestor_mods in
      n.xf := !(n.x) +. current_mods;  

      (* Process left subtree *)
      (match n.l with 
       | Some left -> first_pass_part2 left (!(n.mod_val) :: ancestor_mods)
       | None -> ());

      (* Process right subtree *)
      (match n.r with
       | Some right -> 
           first_pass_part2 right (!(n.mod_val) :: ancestor_mods);

           (* Check conflicts with left subtree *)
           (match n.l with
            | Some left -> 
                let shift = check_subtree_conflicts right left 1.0 in
                if shift > 0.0 then
                  (match right with
                   | Node right_data -> 
                       right_data.shift_val := shift;  
                   | Empty -> ())
            | None -> ())
       | None -> ())

(* Start the first pass with an empty ancestor list for mods *)
let start_first_pass_part2 tree =
  first_pass_part2 tree [] 
  
(* Second Pass *)
let second_pass tree =
  let rec process_node node acc_mod acc_shift =
    match node with
    | Empty -> ()
    | Node {x; mod_val; shift_val; l; r; _} ->
        (* Apply the accumulated shift to the node and its descendants *)
        let final_x = !x +. acc_mod +. acc_shift in
        x := final_x;

        let new_acc_mod = acc_mod +. !mod_val in
        let new_acc_shift = acc_shift +. !shift_val in

        (* Process left and right subtrees, passing the updated mod and shift values *)
        (match l with Some left -> process_node left new_acc_mod new_acc_shift | None -> ());
        (match r with Some right -> process_node right new_acc_mod new_acc_shift | None -> ())
  in
  process_node tree 0.0 0.0


(* Main function *)
let main tree =
  first_pass_p1 tree false None 0;
  start_first_pass_part2 tree;
  second_pass tree;
  ()

(********************************************************************
*********** BELOW IS THE TESTER - DO NOT MODIFY *********************
********************************************************************)

let create_node value x_val y_val mod_val shift_val left right =
  Node {
    v = value;
    x = ref x_val;
    y = ref y_val;
    mod_val = ref mod_val;
    shift_val = ref shift_val;
    xf = ref 0.0;  
    l = left;
    r = right;
  }

(* Test tree creation with shift_val included *)
let test_tree = 
  create_node "A" 0.0 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0 0.0
             (Some (create_node "D" 0.0 0.0 0.0 0.0 None None))
             (Some (create_node "E" 0.0 0.0 0.0 0.0 None None))))
    (Some (create_node "C" 0.0 0.0 0.0 0.0
             (Some (create_node "F" 0.0 0.0 0.0 0.0 None None))
             (Some (create_node "G" 0.0 0.0 0.0 0.0 None None))))

let test_tree2 = 
  create_node "A" 0.0 0.0 0.0 0.0
    None
    (Some (create_node "B" 0.0 0.0 0.0 0.0
             (Some (create_node "E" 0.0 0.0 0.0 0.0 None None))  
             (Some (create_node "C" 0.0 0.0 0.0 0.0
                      None
                      (Some (create_node "D" 0.0 0.0 0.0 0.0 None None))))))

let test_tree3 = 
  create_node "A" 0.0 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0 0.0
             (Some (create_node "C" 0.0 0.0 0.0 0.0
                      (Some (create_node "D" 0.0 0.0 0.0 0.0 None None))
                      None))
             None))
    None


(* Print function *)
let rec print_tree_coords = function
  | Empty -> ()
  | Node {v; x; y; mod_val; shift_val; xf; l; r} ->
      Printf.printf "Node %s: (x=%.1f, y=%.1f, mod=%.1f, shift=%.1f, xf=%.1f)\n" 
        v !x !y !mod_val !shift_val !xf;
      (match l with Some t -> print_tree_coords t | None -> ());
      (match r with Some t -> print_tree_coords t | None -> ())

let () =
  Printf.printf "Before first pass:\n";
  print_tree_coords test_tree;
  
  Printf.printf "\nAfter all passes:\n";
  main test_tree;  (* Modify test_tree name to test different trees*)
  print_tree_coords test_tree

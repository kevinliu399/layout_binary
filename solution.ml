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
  let rec apply_shift (node: 'a tree) (shift: float) : unit =
    match node with
    | Empty -> ()
    | Node {x; mod_val; l; r; _} ->
        x := !x +. shift;
        (match l with Some left -> apply_shift left shift | None -> ());
        (match r with Some right -> apply_shift right shift | None -> ())
  in
  match prev with
  | None -> ()
  | Some parent ->
      match parent.r with
      | Some right -> apply_shift right shift
      | None -> ()

let rec traversal_one (tree: 'a tree) (is_right: bool) (prev: 'a tree option) (depth: int) : unit =
  match tree with
  | Empty -> ()
  | Node n ->
      n.x := (if is_right then !(n.x) +. 1.0 else 0.0);
      n.y := float_of_int depth;
      
      (match n.l with 
       | Some l -> traversal_one l false (Some tree) (depth + 1)
       | None -> ());
      (match n.r with
       | Some r -> traversal_one r true (Some tree) (depth + 1)
       | None -> ());
      
      let children_midpoint = match (n.l, n.r) with
        | (Some l, Some r) -> (get_x l +. get_x r) /. 2.0
        | (Some l, None) -> get_x l
        | (None, Some r) -> get_x r
        | (None, None) -> !(n.x)
      in

      if not is_right && (n.l != None || n.r != None) then
        let original_x = !(n.x) in
        n.x := children_midpoint;
        let shift_amount = children_midpoint -. original_x in
        
        match prev with
        | Some (Node parent) -> 
            (match parent.r with
             | Some (Node right_sibling) ->
                 let new_right_x = !(right_sibling.x) +. shift_amount in
                 right_sibling.x := new_right_x
             | Some Empty | None -> ())
        | None | Some Empty -> ()
      else if is_right && (n.l != None || n.r != None) then
        n.mod_val := !(n.x) -. children_midpoint

let rec traversal_two (tree: 'a tree) (ancestor_mods: float list) : unit =
  
  (* Helper function to check subtree conflicts, using only xf for position *)
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
  in

  match tree with
  | Empty -> ()
  | Node n ->
      let current_mods = List.fold_left (+.) 0.0 ancestor_mods in
      n.xf := !(n.x) +. current_mods;  

      (match n.l with 
       | Some left -> traversal_two left (!(n.mod_val) :: ancestor_mods)
       | None -> ());

      (match n.r with
       | Some right -> 
           traversal_two right (!(n.mod_val) :: ancestor_mods);
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

let traversal_three (tree: 'a tree) : unit =
  let rec process_node node acc_mod acc_shift =
    match node with
    | Empty -> ()
    | Node {x; mod_val; shift_val; l; r; _} ->
        let final_x = !x +. acc_mod +. acc_shift +. !shift_val in
        x := final_x;

        let new_acc_mod = acc_mod +. !mod_val in
        let new_acc_shift = acc_shift +. !shift_val in

        (match l with Some left -> process_node left new_acc_mod new_acc_shift | None -> ());
        (match r with Some right -> process_node right new_acc_mod new_acc_shift | None -> ())
  in
  process_node tree 0.0 0.0

let fix_root (tree: 'a tree) : unit =
  match tree with
  | Empty -> ()
  | Node n -> match (n.l, n.r) with
      | (Some left, Some right) ->
          let left_x = get_x left in
          let right_x = get_x right in
          let root_x = (left_x +. right_x) /. 2.0 in
          n.x := root_x
      | _ -> ()

(********************************************************************
*********** BELOW IS THE TESTER - DO NOT MODIFY *********************
********************************************************************)

let main tree =
  traversal_one tree false None 0;
  traversal_two tree [] ;
  traversal_three tree;
  fix_root tree;
  ()

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

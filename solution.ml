(* By Kevin Liu (261135372), David Zhou(261135446), and Yessine Chaari (261179816) *)

exception NotImplemented

(********************************************************************
******************** TYPES AND HELPER FUNCTIONS *********************
********************************************************************)

(* Datatype Definition *)
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

(* Retrieves node attributes *)
let get_x (tree: 'a tree) : float = 
  match tree with
  | Empty -> 0.0
  | Node n -> !(n.x)

let get_xf tree = 
  match tree with
  | Empty -> 0.0
  | Node n -> !(n.xf)

(* Helpers for traversal 2 *)
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
    shift
  else
    0.0

(********************************************************************
****************** MAIN ALGORITHM - TRAVERSALS **********************
********************************************************************)



let rec traversal_one (tree: 'a tree) (is_right: bool) (prev: 'a tree option) (depth: int) : unit =
  match tree with
  | Empty -> ()
  | Node n ->
      (* Set initial x and y coordinates *)
      n.x := (if is_right then !(n.x) +. 1.0 else 0.0);
      n.y := float_of_int depth;
      
      (* Process children *)
      (match n.l with 
       | Some l -> traversal_one l false (Some tree) (depth + 1)
       | None -> ());
      (match n.r with
       | Some r -> traversal_one r true (Some tree) (depth + 1)
       | None -> ());
      
      (* Calculate children midpoint *)
      let children_midpoint = match (n.l, n.r) with
        | (Some l, Some r) -> (get_x l +. get_x r) /. 2.0
        | (Some l, None) -> get_x l
        | (None, Some r) -> get_x r
        | (None, None) -> !(n.x)
      in

      (* Handle left nodes with children *)
      if not is_right && (n.l != None || n.r != None) then
        let original_x = !(n.x) in
        n.x := children_midpoint;
        let shift_amount = children_midpoint -. original_x in
        
        (* Adjust right sibling's position *)
        match prev with
        | Some (Node parent) -> 
            (match parent.r with
             | Some (Node right_sibling) ->
                 right_sibling.x := !(right_sibling.x) +. shift_amount
             | Some Empty | None -> ())
        | None | Some Empty -> ()

      (* Handle right nodes with children *)
      else if is_right && (n.l != None || n.r != None) then
        n.mod_val := !(n.x) -. children_midpoint



let rec traversal_two (tree: 'a tree) (ancestor_mods: float list) (right_sibling: 'a tree option) : contour = 
  match tree with
  | Empty -> make_empty_contour()
  | Node n ->
      (* Calculate current position including ancestor modifications *)
      let current_mods = List.fold_left (+.) 0.0 ancestor_mods in
      n.xf := !(n.x) +. current_mods;

      (* Initialize contour for this node *)
      let this_contour = {
        leftmost = !(n.xf);
        rightmost = !(n.xf);
        height = 1
      } in
      
      match (n.l, n.r) with
      | (None, None) -> this_contour
      | (Some left, None) ->
          let left_contour = traversal_two left (!(n.mod_val) :: ancestor_mods) None in
          { 
            leftmost = min this_contour.leftmost left_contour.leftmost;
            rightmost = max this_contour.rightmost left_contour.rightmost;
            height = max this_contour.height (left_contour.height + 1)
          }

      | (None, Some right) ->
          let right_contour = traversal_two right (!(n.mod_val) :: ancestor_mods) None in
          {
            leftmost = min this_contour.leftmost right_contour.leftmost;
            rightmost = max this_contour.rightmost right_contour.rightmost;
            height = max this_contour.height (right_contour.height + 1)
          }

      | (Some left, Some right) ->
          let left_contour = traversal_two left (!(n.mod_val) :: ancestor_mods) (Some right) in
          let right_contour = traversal_two right (!(n.mod_val) :: ancestor_mods) None in

          (* Check for conflicts with minimum separation of `shift_distance` *)
          let shift = merge_contours left_contour right_contour shift_distance in
          if shift > 0.0 then (
            match right_sibling with
            | Some (Node right_sibling) -> 
                right_sibling.shift_val := shift_distance;
            | None | Some Empty -> ()
          );

          (* Check for conflicts between subtrees with parent's right sibling *)
          (match right_sibling with
           | Some (Node right_parent) ->
               (match right_parent.l with
                | Some left_of_right ->
                    let left_of_right_contour = 
                      traversal_two left_of_right (!(right_parent.mod_val) :: ancestor_mods) None in
                    let additional_shift = merge_contours right_contour left_of_right_contour 0.5 in
                    if additional_shift > 0.0 then
                      right_parent.shift_val := 0.5  
                | None -> ());
           | None | Some Empty -> ());

          {
            leftmost = min (min this_contour.leftmost left_contour.leftmost) 
                right_contour.leftmost;
            rightmost = max (max this_contour.rightmost left_contour.rightmost)
                right_contour.rightmost;
            height = max this_contour.height 
                (max (left_contour.height + 1) (right_contour.height + 1))
          }



let traversal_three (tree: 'a tree) : unit =
  let rec process_node node acc_mod acc_shift =
    match node with
    | Empty -> ()
    | Node {x; mod_val; shift_val; xf; l; r; _} ->
        let current_shift = acc_shift +. !shift_val in
        
        (* Calculate final position including all modifications and current shift *)
        let final_x = !x +. acc_mod +. current_shift in
        xf := final_x;

        (* Accumulate modifications for children *)
        let new_mod = acc_mod +. !mod_val in
        
        (match l with Some left -> process_node left new_mod current_shift | None -> ());
        (match r with Some right -> process_node right new_mod current_shift | None -> ())
  in
  process_node tree 0.0 0.0

(* Align root to center around its children *)
let fix_root (tree: 'a tree) =
  match tree with
  | Empty -> ()
  | Node n -> match (n.l, n.r) with
    | (Some left, Some right) ->
        let left_x = get_xf left in
        let right_x = get_xf right in 
        let root_x = (left_x +. right_x) /. 2.0 in 
        n.xf := root_x
    | _ -> ()

(********************************************************************
************************** MAIN FUNCTION ***************************
********************************************************************)

let main tree =
  traversal_one tree false None 0;
  traversal_two tree [] None;
  traversal_three tree;
  fix_root tree;
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
  
(* Test 1: Example from https://ocaml.org/exercises *)    
let test_tree = 
  create_node "N" 0.0 0.0 0.0 0.0
    (Some (
        create_node "K" 0.0 0.0 0.0 0.0
          (Some (
              create_node "C" 0.0 0.0 0.0 0.0 
                (Some (
                    create_node "A" 0.0 0.0 0.0 0.0 None None
                  )) 
                (Some (
                    create_node "E" 0.0 0.0 0.0 0.0 
                      (Some (
                          create_node "D" 0.0 0.0 0.0 0.0 None None
                        )) 
                      (Some (
                          create_node "G" 0.0 0.0 0.0 0.0 None None
                        ))
                  ))
            )) 
          (Some (
              create_node "M" 0.0 0.0 0.0 0.0 None None
            ))
      ))
    (Some (
        create_node "U" 0.0 0.0 0.0 0.0
          (Some (
              create_node "P" 0.0 0.0 0.0 0.0 None 
                (Some (
                    create_node "Q" 0.0 0.0 0.0 0.0 None None
                  ))
            ))
          None
      ))
  
(* Test 2: Tree *)
let test_tree2 = 
  create_node "A" 0.0 0.0 0.0 0.0
    None
    (Some (create_node "B" 0.0 0.0 0.0 0.0
             (Some (create_node "E" 0.0 0.0 0.0 0.0 None None))  
             (Some (create_node "C" 0.0 0.0 0.0 0.0
                      None
                      (Some (create_node "D" 0.0 0.0 0.0 0.0 None None))))))
  
(* Test 3: Linked List *)
let test_tree3 = 
  create_node "A" 0.0 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0 0.0
             (Some (create_node "C" 0.0 0.0 0.0 0.0
                      (Some (create_node "D" 0.0 0.0 0.0 0.0 None None))
                      None))
             None))
    None

(* Test 4: Complete tree of depth 2 *)    
let test_tree4 = 
  create_node "A" 0.0 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0 0.0
             (Some (create_node "D" 0.0 0.0 0.0 0.0 None None))
             (Some (create_node "E" 0.0 0.0 0.0 0.0 None None))))
    (Some (create_node "C" 0.0 0.0 0.0 0.0
             (Some (create_node "F" 0.0 0.0 0.0 0.0 None None))
             (Some (create_node "G" 0.0 0.0 0.0 0.0 None None))))

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
  main test_tree;  
  print_tree_coords test_tree ;;

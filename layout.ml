(********************************************************************
******************** Types and helper functions *********************
********************************************************************)
type 'a node_data = {
  v: 'a;
  x: float ref;
  y: float ref;
  mod_val: float ref;
  shift_val: float ref;
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


(* Second Pass *)
let second_pass tree =
  let rec process_node node acc_mod =
    match node with
    | Empty -> ()
    | Node {x; mod_val; l; r; _} ->
        let final_x = !x +. acc_mod in
        x := final_x;
        
        let new_acc_mod = acc_mod +. !mod_val in
        
        (match l with Some left -> process_node left new_acc_mod | None -> ());
        (match r with Some right -> process_node right new_acc_mod | None -> ())
  in
  process_node tree 0.0

(* Main function *)
let main tree =
  first_pass_p1 tree false None 0;
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
    l = left;
    r = right
  }

(* Test tree creation with shift_val included *)
let test_tree = 
  create_node "A" 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0
             (Some (create_node "D" 0.0 0.0 0.0 None None))
             (Some (create_node "E" 0.0 0.0 0.0 None None))))
    (Some (create_node "C" 0.0 0.0 0.0
             (Some (create_node "F" 0.0 0.0 0.0 None None))
             (Some (create_node "G" 0.0 0.0 0.0 None None))))

let test_tree2 = 
  create_node "A" 0.0 0.0 0.0
    None
    (Some (create_node "B" 0.0 0.0 0.0
             (Some (create_node "E" 0.0 0.0 0.0 None None))  
             (Some (create_node "C" 0.0 0.0 0.0
                      None
                      (Some (create_node "D" 0.0 0.0 0.0 None None))))))

let test_tree3 = 
  create_node "A" 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0
             (Some (create_node "C" 0.0 0.0 0.0
                      (Some (create_node "D" 0.0 0.0 0.0 None None))
                      None))
             None))
    None

(* Print function *)
let rec print_tree_coords = function
  | Empty -> ()
  | Node {v; x; y; mod_val; shift_val; l; r} ->
      Printf.printf "Node %s: (x=%.1f, y=%.1f, mod=%.1f, shift=%.1f)\n" 
        v !x !y !mod_val !shift_val;
      (match l with Some t -> print_tree_coords t | None -> ());
      (match r with Some t -> print_tree_coords t | None -> ())
      

let () =
  Printf.printf "Before first pass:\n";
  print_tree_coords test_tree;
  
  Printf.printf "\nAfter all passes:\n";
  main test_tree;  (* Modify test_tree name to test different trees*)
  print_tree_coords test_tree

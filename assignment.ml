(* By Kevin Liu (261136372), David Zhou(261135446), and Yessine Chaari (261179816) *)

(********************************************************************
******************** TYPES AND HELPER FUNCTIONS *********************
********************************************************************)
type 'a node_data = {
  v: 'a;
  x: float ref;
  y: float ref;
  mod_val: float ref;
  l: 'a tree option;
  r: 'a tree option
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

(* Exercise 1 : Assign initial value *)
let rec first_pass_p1 tree is_right prev depth =
  raise NotImplemented


(* Second Pass *)
let second_pass tree =
  raise NotImplemented


(********************************************************************
************** BELOW IS SOME TREES TO HELP YOU  *********************
********************************************************************)

let main tree =
  first_pass_p1 tree false None 0;
  second_pass tree;
  ()

let create_node value x_val y_val mod_val left right =
  Node {
    v = value;
    x = ref x_val;
    y = ref y_val;
    mod_val = ref mod_val;
    l = left;
    r = right
  }

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

let test_tree = 
  create_node "A" 0.0 0.0 0.0
    (Some (create_node "B" 0.0 0.0 0.0
             (Some (create_node "C" 0.0 0.0 0.0
                      (Some (create_node "D" 0.0 0.0 0.0 None None))
                      None))
             None))
    None

let rec print_tree_coords = function
  | Empty -> ()
  | Node {v; x; y; mod_val; l; r} ->
      Printf.printf "Node %s: (x=%.1f, y=%.1f, mod=%.1f)\n" 
        v !x !y !mod_val;
      (match l with Some t -> print_tree_coords t | None -> ());
      (match r with Some t -> print_tree_coords t | None -> ())

let () =
  Printf.printf "Before first pass:\n";
  print_tree_coords test_tree;
  
  Printf.printf "\nAfter all passes:\n";
  main test_tree;  
  print_tree_coords test_tree
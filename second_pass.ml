(* LEGACY FILE FOR SECOND/THIRD PASS THAT IS TESTED AND WORKING ON ITS OWN *)
(* THERE MIGHT BE CONFLICTS WITH THE MERGED CODE SO THIS IS JUST TO HAVE THE LEGACY CODE *)

(* First define minimal required types and functions *)
type 'a tree = 
  | Empty 
  | Node of { 
      v: 'a; 
      x: float ref; 
      y: float ref; 
      mod_val: float ref; 
      l: 'a tree option; 
      r: 'a tree option 
    }

let rec apply_shift node shift =
  match node with
  | Empty -> ()
  | Node {x; mod_val; l; r; _} ->
      x := !x +. shift;
      (match l with Some left -> apply_shift left shift | None -> ());
      (match r with Some right -> apply_shift right shift | None -> ())

let second_pass tree =
  (* Track if smallest x is negative *)
  let min_x = ref 0.0 in
  let rec process_node node acc_mod =
    match node with
    | Empty -> ()
    | Node {x; mod_val; l; r; _} ->
        let final_x = !x +. acc_mod in
        x := final_x;
        
        if final_x < !min_x then
          min_x := final_x; 
        
        (* Stack mod val *)
        let new_acc_mod = acc_mod +. !mod_val in
        
        (* Recursive call *)
        (match l with 
         | Some left -> process_node left new_acc_mod 
         | None -> ());
        (match r with 
         | Some right -> process_node right new_acc_mod 
         | None -> ())
  in

  process_node tree 0.0;
  !min_x

(* Fix negative value if necessary *)
let normalize_coordinates tree min_x =
  if min_x < 0.0 then
    apply_shift tree (abs_float min_x)

(* Main Call *)
let main tree =
  let min_x = second_pass tree in
  normalize_coordinates tree min_x

(* TEST CASE FOR SECOND TRAVERSAL *)
let create_node value x_val y_val mod_val left right =
  Node {
    v = value;
    x = ref x_val;
    y = ref y_val;
    mod_val = ref mod_val;
    l = left;
    r = right
  }

(* Test tree with pre-set coordinates and modifiers *)
let test_tree = 
  create_node 'A' 2.0 0.0 1.0
    (Some (create_node 'B' 1.0 1.0 (-0.5)
             (Some (create_node 'D' 0.0 2.0 0.0 None None))
             (Some (create_node 'E' 2.0 2.0 0.0 None None))))
    (Some (create_node 'C' 4.0 1.0 0.5
             None
             (Some (create_node 'F' 5.0 2.0 0.0 None None))))

(* Function to print tree coordinates *)
let rec print_tree_coords = function
  | Empty -> ()
  | Node {v; x; y; mod_val; l; r} ->
      Printf.printf "Node %c: (x=%.1f, y=%.1f, mod=%.1f)\n" 
        v !x !y !mod_val;
      (match l with Some t -> print_tree_coords t | None -> ());
      (match r with Some t -> print_tree_coords t | None -> ())

(* Run just the second pass *)
let () =
  Printf.printf "Before second pass:\n";
  print_tree_coords test_tree;
  
  Printf.printf "\nAfter second pass and normalization:\n";
  main test_tree;
  print_tree_coords test_tree

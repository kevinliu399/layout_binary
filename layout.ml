(* By Kevin Liu, Yessine Chaari, David Zhou*)

(****************************
*  Type and helper function *
*****************************)

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

(* For testing purposes *)
let rec print_positions node =
  match node with
  | Empty -> ()
  | Node {v; x; y; mod_val; l; r} ->
      Printf.printf "Node %s: x = %.2f, y = %.2f, mod_val = %.2f\n" 
        v !x !y !mod_val;
      (match l with Some left -> print_positions left | None -> ());
      (match r with Some right -> print_positions right | None -> ())


(* Set sibling distance to 1 *)
let sibling_distance = 1.0

let rec apply_shift node shift =
  match node with
  | Empty -> ()
  | Node {x; mod_val; l; r; _} ->
      x := !x +. shift;
      (match l with Some left -> apply_shift left shift | None -> ());
      (match r with Some right -> apply_shift right shift | None -> ())

let get_x tree = match tree with
  | Empty -> 0.0
  | Node n -> !(n.x)

(******************
*  Main Algorithm *
*******************)

let rec first_pass_p1 tree is_right prev =
  match tree with
  | Empty -> ()
  | Node n ->
      (* Step 1: Set initial x based on whether it's a right child *)
      n.x := (if is_right then 1.0 else 0.0);
      
      (* Process children *)
      (match n.l with 
      | Some l -> first_pass_p1 l false (Some tree)
      | None -> ());
      (match n.r with
      | Some r -> first_pass_p1 r true (Some tree)
      | None -> ());
      
      (* Step 2: If node has children, calculate children's midpoint *)
      let children_midpoint = match (n.l, n.r) with
        | (Some l, Some r) -> (get_x l +. get_x r) /. 2.0
        | (Some l, None) -> get_x l
        | (None, Some r) -> get_x r
        | (None, None) -> !(n.x)
      in

      (* Step 3: Handle the two special cases *)
      if prev = None && (n.l != None || n.r != None) then
        (* Case 1: Leftmost node with children - center it *)
        n.x := children_midpoint
      else if prev != None && (n.l != None || n.r != None) then
        (* Case 2: Non-leftmost node with children - set mod *)
        n.mod_val := !(n.x) -. children_midpoint

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

(* Third Pass *)
(* Fix negative value if necessary *)
let normalize_coordinates tree min_x =
  if min_x < 0.0 then
    apply_shift tree (abs_float min_x)

(* Main Call *)
let main tree =
  let min_x = second_pass tree in
  normalize_coordinates tree min_x

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


let get_x tree = match tree with
  | Empty -> 0.0
  | Node n -> !(n.x)

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



let make_leaf v = Node {
  v = v;
  x = ref 0.0;
  y = ref 0.0;
  mod_val = ref 0.0;
  l = None;
  r = None
}

let make_node v left right = Node {
  v = v;
  x = ref 0.0;
  y = ref 0.0;
  mod_val = ref 0.0;
  l = Some left;
  r = Some right
}

(* Helper to print test results *)
let test name expected actual =
  if abs_float (expected -. actual) < 0.001 then
    Printf.printf "PASS: %s\n" name
  else
    Printf.printf "FAIL: %s (Expected: %f, Got: %f)\n" name expected actual

(* Test cases *)
let run_tests () =

  (* Test 1 *)
  let t1 = make_leaf 'a' in
  first_pass_p1 t1 false None;
  test "Single node x value" 0.0 (get_x t1);
  test "Single node mod value" 0.0 !(match t1 with Node n -> n.mod_val | Empty -> ref 0.0);

  (* Test 2 *)
  let t2 = Node {
    v = 'a';
    x = ref 0.0;
    y = ref 0.0;
    mod_val = ref 0.0;
    l = None;
    r = Some (make_leaf 'b')
  } in
  first_pass_p1 t2 false None;
  test "Parent with right child - parent x" 1.0 (get_x t2);
  (match t2 with 
   | Node n -> (match n.r with 
               | Some r -> test "Right child x value" 1.0 (get_x r)
               | None -> ())
   | Empty -> ());

  (* Test 3 *)
  let t3 = make_node 'a' (make_leaf 'b') (make_leaf 'c') in
  first_pass_p1 t3 false None;
  test "Parent with both children - parent x" 0.5 (get_x t3);
  (match t3 with
   | Node n -> 
       (match n.l with
        | Some l -> test "Left child x value" 0.0 (get_x l)
        | None -> ());
       (match n.r with
        | Some r -> test "Right child x value" 1.0 (get_x r)
        | None -> ())
   | Empty -> ());

  (* Test 4 *)
  let t4 = make_node 'a'
    (Node {
      v = 'b';
      x = ref 0.0;
      y = ref 0.0;
      mod_val = ref 0.0;
      l = Some (make_leaf 'd');
      r = None
    })
    (Node {
      v = 'c';
      x = ref 0.0;
      y = ref 0.0;
      mod_val = ref 0.0;
      l = None;
      r = Some (make_leaf 'e')
    }) in
  first_pass_p1 t4 false None;
  test "Complex tree - root x" 0.5 (get_x t4);
  (match t4 with
   | Node n -> 
       (match n.l with
        | Some l -> test "Complex tree - b node x" 0.0 (get_x l)
        | None -> ());
       (match n.r with
        | Some r -> test "Complex tree - c node x" 1.0 (get_x r)
        | None -> ())
   | Empty -> ());;

(* Run all tests *)
let () = run_tests ()


(*
Results as of 2024-11-12

PASS: Single node x value
PASS: Single node mod value
PASS: Parent with right child - parent x
PASS: Right child x value
PASS: Parent with both children - parent x
PASS: Left child x value
PASS: Right child x value
PASS: Complex tree - root x
PASS: Complex tree - b node x
PASS: Complex tree - c node x

*)
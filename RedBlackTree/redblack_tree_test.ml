open OUnit
open Redblack_tree

let rec insert_list t l = match l with
  | x::xs -> insert_list (insert x t) xs
  | [] -> t

let rec insert_times t n =
  if n > 0 then insert_times (insert (Random.int 10000000) t) (n - 1)
  else t

let test1 _ =
  let rbt = insert_times E 1000000 in
  let rbt0 = insert 0 rbt in
  assert (member 0 rbt0)

let suite = "Test RedBlackTree" >:::
  ["test1" >:: test1]

let _ =
  Random.init 1;
  run_test_tt_main suite

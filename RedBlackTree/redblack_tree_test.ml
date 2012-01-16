open OUnit
open Redblack_tree

let test1 _ =
  let rec f t l = match l with
    | x::xs -> f (insert x t) xs
    | [] -> t in
  let rbt = f E [3;2;4;5;6;9;0] in 
  assert (member 0 rbt)

let suite = "Test SmallStream" >:::
  ["test1"  >:: test1;
  ]

let _ = run_test_tt_main suite

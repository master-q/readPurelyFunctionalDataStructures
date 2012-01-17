open OUnit

let rec insert_list fi t l = match l with
  | x::xs -> insert_list fi (fi x t) xs
  | [] -> t

let rec insert_times fi t n =
  if n > 0 then insert_times fi (fi (Random.int 10000000) t) (n - 1)
  else t

let test1 fi fm te _ =
  let rbt = insert_times fi te 1000000 in
  let rbt0 = fi 0 rbt in
  assert (fm 0 rbt0)

let suite fi fm te = "Test RedBlackTree" >:::
  ["test1" >:: (test1 fi fm te)]

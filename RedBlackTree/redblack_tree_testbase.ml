open OUnit

let rec insert_list fi t l = match l with
  | x::xs -> insert_list fi (fi x t) xs
  | [] -> t

let rec insert_times_rand fi t n =
  if n > 0 then insert_times_rand fi (fi (Random.int 10000000) t) (n - 1)
  else t

let rec insert_times_incl fi t until n =
  if n < until then insert_times_incl fi (fi n t) until (n + 1)
  else t

let rec insert_times_decl fi t n =
  if n >= 0 then insert_times_decl fi (fi n t) (n - 1)
  else t

let test_rand fi fm te _ =
  let rbt = insert_times_rand fi te 1000000 in
  let rbt0 = fi 0 rbt in
  assert (fm 0 rbt0)

let test_sorted fi fm te _ =
  let rbt = insert_times_incl fi te 1000000 0 in
  assert (fm 0 rbt)

let test_rsorted fi fm te _ =
  let rbt = insert_times_decl fi te 1000000 in
  assert (fm 0 rbt)

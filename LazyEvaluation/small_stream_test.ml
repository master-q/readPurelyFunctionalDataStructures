open OUnit
open Small_stream

let stream_321 =
  lazy (SScons(3, (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))))
let stream_933 =
  lazy (SScons(9, (lazy (SScons(3, lazy (SScons(3, lazy SSnil)))))))
let stream_inf = repeat 1
let stream_inf2 = repeat 2
let stream_inc = iterate ((+) 1) 0

let test_repeat _ = match drop 100000 stream_inf with
  | (lazy SSnil) -> assert_failure "stream_inf has no SSnil."
  | (lazy (SScons(x, _))) -> assert_equal 1 x

let test_drop _ = match drop 2 stream_321 with
  | (lazy SSnil) -> assert_failure "stream has only one elm."
  | (lazy (SScons(x, _))) -> assert_equal 1 x

let test_take _ = match drop 3 (take 4 stream_inf) with
  | (lazy SSnil) -> assert_failure "stream_inf has no SSnil."
  | (lazy (SScons(x, _))) -> assert_equal 1 x

let test_append _ = match drop 9 (take 10 (stream_inf2 ++ stream_inf)) with
  | (lazy SSnil) -> assert_failure "stream_inf has no SSnil."
  | (lazy (SScons(x, _))) -> assert_equal 2 x

let test_reverse _ = match reverse (reverse stream_321) with
  | (lazy SSnil) -> assert_failure "stream has elms."
  | (lazy (SScons(x, _))) -> assert_equal 3 x

let test_foldl _ = assert_equal 1 (foldl (/) 81 stream_933)

let test_foldl1 _ = assert_equal 1 (foldl1 (/) stream_933)

let test_foldl1_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (foldl1 (+) (lazy SSnil)))

let test_sum _ = assert_equal 6 (sum stream_321)

let test_iterate _ = assert_equal 45 (sum (take 10 stream_inc))

let test_foldr _ = assert_equal 9 (foldr (lift_foldr (/)) 1 stream_933)

let test_foldr_2 _ = let plus_until a b =
		       if a > 6 then a else (lift_foldr (+)) a b
		     in assert_equal 28 (foldr plus_until 0 stream_inc)

let test_foldr1 _ = assert_equal 9 (foldr1 (lift_foldr (/)) stream_933)

let test_foldr1_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (foldr1 (lift_foldr (+)) (lazy SSnil)))

let suite = "Test SmallStream" >:::
  ["test_repeat"  >:: test_repeat;
   "test_drop"    >:: test_drop;
   "test_take"    >:: test_take;
   "test_append"  >:: test_append;
   "test_reverse" >:: test_reverse;
   "test_foldr"   >:: test_foldr;
   "test_foldr_2" >:: test_foldr_2;
   "test_foldl"   >:: test_foldl;
   "test_foldl1"  >:: test_foldl1;
   "test_foldl1_2" >:: test_foldl1_2;
   "test_foldr1"  >:: test_foldr1;
   "test_foldr1_2" >:: test_foldr1_2;
   "test_sum"     >:: test_sum;
   "test_iterate" >:: test_iterate]

let _ = run_test_tt_main suite

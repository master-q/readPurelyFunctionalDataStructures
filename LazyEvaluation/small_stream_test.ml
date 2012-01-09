open OUnit
open Small_stream

let stream_321 =
  lazy (SScons(3, (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))))
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

let test_foldr _ = assert_equal 6 (foldr (lift_foldr (+)) 0 stream_321)

let plus_until a b = if a > 6 then a else (lift_foldr (+)) a b

let test_foldr_2 _ = assert_equal 28 (foldr plus_until 0 stream_inc)

let suite = "Test SmallStream" >:::
  ["test_repeat" >:: test_repeat;
   "test_drop"   >:: test_drop;
   "test_take"   >:: test_take;
   "test_append" >:: test_append;
   "test_reverse" >:: test_reverse;
   "test_foldr"  >:: test_foldr;
   "test_foldr_2" >:: test_foldr_2]
(** need iterate test **)

let _ = run_test_tt_main suite

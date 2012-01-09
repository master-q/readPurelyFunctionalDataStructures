open OUnit
open Small_stream

let stream_321 =
  lazy (SScons(3, (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))))
let stream_inf = repeat 1
let stream_inf2 = repeat 2
let stream_inf3 = repeat 3

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

let plus a b = match b with
  | (lazy SSnil) -> (a, SSnil)
  | (lazy (SScons(x, s))) -> (a + x, SScons(x, s))

let test_foldr _ = assert_equal 6 (foldr plus 0 stream_321)

let plus_until a b = match b with
  | (lazy SSnil) -> (a, SSnil)
  | (lazy (SScons(x, s))) -> if a > 6 then (a, SSnil) else (a + x, SScons(x, s))

let test_foldr_2 _ = assert_equal 9 (foldr plus_until 0 stream_inf3)

let suite = "Test SmallStream" >:::
  ["test_repeat" >:: test_repeat;
   "test_drop"   >:: test_drop;
   "test_take"   >:: test_take;
   "test_append" >:: test_append;
   "test_reverse" >:: test_reverse;
   "test_foldr"  >:: test_foldr;
   "test_foldr_2" >:: test_foldr_2]

let _ = run_test_tt_main suite

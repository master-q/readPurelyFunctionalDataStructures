open OUnit
open Small_stream

let stream_321 =
  lazy (SScons(3, (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))))

let stream_inf = repeat 1

let test_0 _ = match drop 100000 stream_inf with
  | (lazy SSnil) -> assert_failure "stream_inf has no SSnil."
  | (lazy (SScons(x, _))) -> assert_equal 1 x

let test_1 _ = let st = drop 1 (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))
	       in match st with
		 | (lazy SSnil) -> assert_failure "stream has only one elm."
		 | (lazy (SScons(x, _))) -> assert_equal 1 x

let suite = "Test SmallStream" >:::
  ["test_0" >:: test_0;
   "test_1" >:: test_1]

let _ = run_test_tt_main suite

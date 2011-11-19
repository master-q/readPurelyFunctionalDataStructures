open OUnit
open Small_stream

let test_1 _ = let st = drop 1 (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))
	       in match Lazy.force st with
		 | (lazy SSnil) -> assert_equal 0 1 (* error *)
		 | (lazy (SScons(x, _))) -> assert_equal 2 x (* why??? *)

let suite = "Test SmallStream" >:::
  ["test_1" >:: test_1]

let _ = run_test_tt_main suite

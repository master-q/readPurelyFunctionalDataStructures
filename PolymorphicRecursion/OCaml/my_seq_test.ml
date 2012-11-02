open OUnit
open Printf
open My_seq

let test_0 _ = let len = sizeS Nil
	       in assert_equal 0 len

let test_1 _ = let len = sizeS (Cons(1, Nil))
	       in assert_equal 1 len

let test_3 _ = let len = sizeS (Cons(1, Cons((2, 3), Nil)))
	       in assert_equal 3 len

let suite = "Test MySeq" >:::
  ["test_0" >:: test_0;
   "test_1" >:: test_1;
   "test_3" >:: test_3]

let _ = run_test_tt_main suite

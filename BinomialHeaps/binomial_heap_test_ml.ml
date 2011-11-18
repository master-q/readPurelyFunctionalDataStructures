open OUnit
open Binomial_heap_ml

let test_1 _ = let found = BinomialHeap.findMin
		 (BinomialHeap.insertAll [2;3;1;4;5;9;8;7;6] [])
	       in assert_equal 1 found

let test_2 _ = let found = BinomialHeap.findMin'
		 (BinomialHeap.insertAll [2;3;1;4;5;9;8;7;6] [])
	       in assert_equal 1 found

let suite = "Test BinomialHeap" >:::
  ["test_1" >:: test_1;
   "test_2" >:: test_2]

let _ = run_test_tt_main suite

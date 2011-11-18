open OUnit
open Binomial_heap_ml

let test_1 _ = let found = BinomialHeapInt.findMin
(*		 (BinomialHeapInt.insertAll [2;3;1;4;5;9;8;7;6] []) *)
		 (BinomialHeapInt.insertAll [1;2;3] [])
	       in assert_equal 1 found

let suite = "Test BinomialHeap" >:::
  ["test_1" >:: test_1]

let _ =
  run_test_tt_main suite

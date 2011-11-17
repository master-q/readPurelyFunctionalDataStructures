open OUnit
open Binomial_heap_ml

let test_1 _ =
  assert_equal BinomialHeapInt.empty []

let suite = "Test BinomialHeap" >:::
  ["test_1" >:: test_1]

let _ =
  run_test_tt_main suite

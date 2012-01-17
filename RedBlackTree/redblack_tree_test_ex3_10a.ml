open OUnit
open Redblack_tree_ex3_10a
open Redblack_tree_testbase

let _ =
  Random.init 1;
  run_test_tt_main (suite insert member E)

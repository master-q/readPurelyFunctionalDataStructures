open OUnit
open Realtime_queue

let test_123 _ =
  let q123 = RTQueue.snoc (RTQueue.snoc (RTQueue.snoc RTQueue.empty 1) 2) 3 in
  let q23 = RTQueue.tail q123 in
  let q3 = RTQueue.tail q23 in
  assert_equal 1 (RTQueue.head q123);
  assert_equal 2 (RTQueue.head q23);
  assert_equal 3 (RTQueue.head q3)

let suite = "Test SmallStream" >:::
  ["test_123"  >:: test_123;
  ]

let _ = run_test_tt_main suite

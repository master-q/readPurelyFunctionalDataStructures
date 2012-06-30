open OUnit
open Printf
open Realtime_queue

let test_123 _ =
  let q1 = RTQueue.snoc RTQueue.empty 1 in
  let q12 = RTQueue.snoc q1 2 in
  let q123 = RTQueue.snoc q12 3 in
  let q1234 = RTQueue.snoc q123 4 in
  let q12345 = RTQueue.snoc q1234 5 in
  let q2345 = RTQueue.tail q12345 in
  let q345 = RTQueue.tail q2345 in
  let q45 = RTQueue.tail q345 in
  let q5 = RTQueue.tail q45 in
  printf "%s\n" (RTQueue.show q1);
  printf "%s\n" (RTQueue.show q12);
  printf "%s\n" (RTQueue.show q123);
  printf "%s\n" (RTQueue.show q1234);
  printf "%s\n" (RTQueue.show q12345);
  printf "%s\n" (RTQueue.show q2345);
  printf "%s\n" (RTQueue.show q345);
  printf "%s\n" (RTQueue.show q45);
  printf "%s\n" (RTQueue.show q5);
  assert_equal 1 (RTQueue.head q12345);
  assert_equal 2 (RTQueue.head q2345);
  assert_equal 3 (RTQueue.head q345);
  assert_equal 4 (RTQueue.head q45);
  assert_equal 5 (RTQueue.head q5)

let suite = "Test SmallStream" >:::
  ["test_123"  >:: test_123;
  ]

let _ = run_test_tt_main suite

open OUnit
open Small_stream

let stream_321 =
  lazy (SScons(3, (lazy (SScons(2, lazy (SScons(1, lazy SSnil)))))))
let stream_933 =
  lazy (SScons(9, (lazy (SScons(3, lazy (SScons(3, lazy SSnil)))))))
let stream_ttt =
  lazy (SScons(true, (lazy (SScons(true, lazy (SScons(true, lazy SSnil)))))))
let stream_ftf =
  lazy (SScons(false, (lazy (SScons(true, lazy (SScons(false, lazy SSnil)))))))
let stream_inf = repeat 1
let stream_inf2 = repeat 2
let stream_inc = iterate ((+) 1) 0
let sstream_321_933 =
  lazy (SScons(stream_321, lazy (SScons(stream_933, lazy SSnil))))

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

let test_foldl _ = assert_equal 1 (foldl (/) 81 stream_933)

let test_foldl1 _ = assert_equal 1 (foldl1 (/) stream_933)

let test_foldl1_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (foldl1 (+) (lazy SSnil)))

let test_sum _ = assert_equal 6 (sum stream_321)

let test_iterate _ = assert_equal 45 (sum (take 10 stream_inc))

let test_foldr _ = assert_equal 9 (foldr (lift_foldr (/)) 1 stream_933)

let test_foldr_2 _ = let plus_until a b =
		       if a > 6 then a else (lift_foldr (+)) a b
		     in assert_equal 28 (foldr plus_until 0 stream_inc)

let test_foldr1 _ = assert_equal 9 (foldr1 (lift_foldr (/)) stream_933)

let test_foldr1_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (foldr1 (lift_foldr (+)) (lazy SSnil)))

let test_map _ = assert_equal 20 (sum (take 10 (map ((+) 1) stream_inf)))

let test_filter _ = assert_equal 18 (sum (filter ((<) 2) (stream_933 ++ stream_321)))

let test_head _ = assert_equal 2 (head stream_inf2)

let test_head_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (head (lazy SSnil)))

let test_last _ = assert_equal 1 (last stream_321)

let test_last_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (last (lazy SSnil)))

let test_tail _ = assert_equal 1 (head (tail stream_inc))

let test_tail_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (sum (tail (lazy SSnil))))

let test_init _ = assert_equal 45 (sum (take 10 (init stream_inc)))

let test_init_2 _ = assert_raises Empty_stream
  (fun _ -> assert_equal 0 (sum (init (lazy SSnil))))

let test_null _ =
  assert_bool "empty stream" (null (lazy SSnil));
  assert_bool "not empty stream" (not (null stream_inf))

let test_length _ =
  assert_equal 0 (length (lazy SSnil));
  assert_equal 3 (length stream_321)

let test_index _ = assert_equal 10 ((!!) stream_inc 10)

let test_index_2 _ =
  assert_raises Empty_stream
    (fun _ -> assert_equal 0 ((!!) stream_inf (-1)));
  assert_raises Empty_stream
    (fun _ -> assert_equal 0 ((!!) stream_321 3))

let test_sand _ = assert_bool "stream has true cell only" (sand stream_ttt)

let test_sor _ = assert_bool "stream has false cell only" (sor stream_ftf)

let test_any _ =
  assert_bool "stream does not have 4" (not (any ((=) 4) stream_321));
  assert_bool "stream has cells < 2" (any ((>) 2) stream_321)

let test_all _ =
  assert_bool "stream has cell 9" (not (all ((=) 3) stream_933));
  assert_bool "stream has cells > 4" (all ((>) 4) stream_321)

let test_product _ = assert_equal 81 (product stream_933)

let test_concat _ = assert_equal 21 (sum (concat sstream_321_933))

let test_concatMap _ =
  let f x = lazy (SScons(x + 1, lazy (SScons(x + 2, lazy SSnil))))
  in assert_equal 21 (sum (concatMap f stream_321))

let test_maximum _ = assert_equal 3 (maximum stream_321)

let test_minimum _ = assert_equal 1 (minimum stream_321)

let test_scanl _ = assert_equal 22 (sum (scanl (+) 2 stream_321))

let test_scanl1 _ = assert_equal 14 (sum (scanl1 (+) stream_321))

let test_scanr _ = assert_equal 18 (sum (scanr (+) 2 stream_321))

let test_scanr1 _ = assert_equal 10 (sum (scanr1 (+) stream_321))

let test_replicate _ = assert_equal 10 (sum (replicate 10 1))

let test_cycle _ = assert_equal 21 (sum (take 10 (cycle stream_321)))

let test_splitAt _ = let (a, b) = splitAt 2 stream_321 in
		     assert_equal 5 (sum a);
		     assert_equal 1 (sum b)

let test_takeWhile _ = assert_equal 45 (sum (takeWhile ((>) 10) stream_inc))

let test_dropWhile _ =
  assert_equal 145 (sum (take 10 (dropWhile ((>) 10) stream_inc)))

let test_span _ = let (a, b) = span ((<) 2) (stream_933 ++ stream_321) in
		  assert_equal 18 (sum a);
		  assert_equal 3 (sum b)

let test_break _ = let (a, b) = break ((<) 4) (stream_321 ++ stream_933) in
		   assert_equal 6 (sum a);
		   assert_equal 15 (sum b)

let test_elem _ =
  assert_bool "should find 3 in stream_321" (elem 3 stream_321);
  assert_bool "should not find 1 in stream_933" (not (elem 1 stream_933))

let test_notElem _ =
  assert_bool "should find 3 in stream_321" (not (notElem 3 stream_321));
  assert_bool "should not find 1 in stream_933" (notElem 1 stream_933)

let test_lookup _ = let alist = map (fun x -> (x, x + 1)) stream_inc
		    in assert_equal (Some 11) (lookup 10 alist)

let test_zip _ = assert_bool "TODO" false
let test_zip3 _ = assert_bool "TODO" false
let test_zipWith _ = assert_bool "TODO" false
let test_zipWith3 _ = assert_bool "TODO" false
let test_unzip _ = assert_bool "TODO" false
let test_unzip3 _ = assert_bool "TODO" false
let test_unfoldr _ = assert_bool "TODO" false

let suite = "Test SmallStream" >:::
  ["test_repeat"  >:: test_repeat;
   "test_drop"    >:: test_drop;
   "test_take"    >:: test_take;
   "test_append"  >:: test_append;
   "test_reverse" >:: test_reverse;
   "test_foldr"   >:: test_foldr;
   "test_foldr_2" >:: test_foldr_2;
   "test_foldl"   >:: test_foldl;
   "test_foldl1"  >:: test_foldl1;
   "test_foldl1_2" >:: test_foldl1_2;
   "test_foldr1"  >:: test_foldr1;
   "test_foldr1_2" >:: test_foldr1_2;
   "test_sum"     >:: test_sum;
   "test_iterate" >:: test_iterate;
   "test_map"     >:: test_map;
   "test_filter"  >:: test_filter;
   "test_head"    >:: test_head;
   "test_head_2"  >:: test_head_2;
   "test_last"    >:: test_last;
   "test_last_2"  >:: test_last_2;
   "test_tail"    >:: test_tail;
   "test_tail_2"  >:: test_tail_2;
   "test_init"    >:: test_init;
   "test_init_2"  >:: test_init_2;
   "test_null"    >:: test_null;
   "test_length"  >:: test_length;
   "test_index"   >:: test_index;
   "test_index_2" >:: test_index_2;
   "test_sand"    >:: test_sand;
   "test_sor"     >:: test_sor;
   "test_any"     >:: test_any;
   "test_all"     >:: test_all;
   "test_product" >:: test_product;
   "test_concat"  >:: test_concat;
   "test_concatMap" >:: test_concatMap;
   "test_maximum" >:: test_maximum;
   "test_minimum" >:: test_minimum;
   "test_scanl"   >:: test_scanl;
   "test_scanl1"  >:: test_scanl1;
   "test_scanr"   >:: test_scanr;
   "test_scanr1"  >:: test_scanr1;
   "test_replicate" >:: test_replicate;
   "test_cycle"   >:: test_cycle;
   "test_splitAt" >:: test_splitAt;
   "test_takeWhile" >:: test_takeWhile;
   "test_dropWhile" >:: test_dropWhile;
   "test_span"    >:: test_span;
   "test_break"   >:: test_break;
   "test_elem"    >:: test_elem;
   "test_notElem" >:: test_notElem;
   "test_lookup"  >:: test_lookup;
   "test_zip"     >:: test_zip;
   "test_zip3"    >:: test_zip3;
   "test_zipWith" >:: test_zipWith;
   "test_zipWith3" >:: test_zipWith3;
   "test_unzip"   >:: test_unzip;
   "test_unzip3"  >:: test_unzip3;
   "test_unfoldr"  >:: test_unfoldr;
  ]

let _ = run_test_tt_main suite

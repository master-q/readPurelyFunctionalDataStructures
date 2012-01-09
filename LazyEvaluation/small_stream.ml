type 'a cell = SSnil | SScons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

exception Empty_stream

(*
http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html
*)
(* *** List operations *)
let rec map f s = lazy (match s with
  | (lazy SSnil) -> SSnil
  | (lazy (SScons(x, s))) -> SScons(f x, map f s))

let rec (++) t1 t2 = lazy (match (t1, t2) with
  | (lazy SSnil, lazy t2) -> t2
  | (lazy (SScons(x, s)), t2) -> SScons(x, s ++ t2))

let rec filter f s = lazy (match s with
  | (lazy SSnil) -> SSnil
  | (lazy (SScons(x, s))) ->
    if f x then SScons(x, filter f s) else Lazy.force (filter f s))

let head s = match s with
  | (lazy (SScons(x, _))) -> x
  | (lazy SSnil) -> raise Empty_stream

let rec last s = match s with
  | (lazy (SScons(x, lazy SSnil))) -> x
  | (lazy (SScons(x, s)))          -> last s
  | (lazy SSnil)                   -> raise Empty_stream

let tail s = match s with
  | (lazy (SScons(x, s))) -> s
  | (lazy SSnil)          -> raise Empty_stream

let rec init s = lazy(match s with
  | (lazy (SScons(_, lazy SSnil))) -> SSnil
  | (lazy (SScons(x, s)))          -> SScons(x, init s)
  | (lazy SSnil)                   -> raise Empty_stream)

let null = function
  | (lazy SSnil)          -> true
  | (lazy (SScons(_, _))) -> false

let length s =
  let rec length s a = match s with
    | (lazy SSnil) -> a
    | (lazy (SScons(_, s))) -> length s (a + 1)
  in length s 0

let rec (!!) s n = match (s, n) with
  | (_, n) when n < 0        -> raise Empty_stream
  | (lazy SSnil, _)          -> raise Empty_stream
  | (lazy (SScons(x, _)), 0) -> x
  | (lazy (SScons(_, s)), n) -> (!!) s (n - 1)

let reverse s = lazy (
  let rec reverse' s r = match (s, r) with
    | (lazy SSnil, r) -> r
    | (lazy (SScons(x, s)), r) -> reverse' s (SScons(x, lazy r))
  in reverse' s SSnil)

(* *** Reducing lists (folds) *)
let rec foldl f z s = match s with
  | (lazy SSnil)          -> z
  | (lazy (SScons(x, s))) -> foldl f (f z x) s

let foldl1 f s = match s with
  | (lazy SSnil)          -> raise Empty_stream
  | (lazy (SScons(x, s))) -> foldl f x s

let lift_foldr f a b = f a (Lazy.force b)

let foldr f z s =
  let rec foldr' f z s = lazy (match s with
    | (lazy SSnil)          -> z
    | (lazy (SScons(x, s))) -> f x (foldr' f z s))
  in Lazy.force (foldr' f z s)

let foldr1 f s =
  let rec foldr1 f s = lazy (match s with
    | (lazy (SScons(x, lazy SSnil))) -> x
    | (lazy (SScons(x, s)))          -> f x (foldr1 f s)
    | _                              -> raise Empty_stream)
  in Lazy.force (foldr1 f s)

(* *** Special folds *)
(* and *)
let sand = foldl (&&) true

(* or *)
let sor = foldl (||) false

let any f s = sor (map f s)

let all f s = sand (map f s)

let sum = foldl (+) 0
let sum' = foldl (+.) 0.0

let product = foldl ( * ) 1
let product' = foldl ( *.) 1.0

let rec concat ss = match ss with
  | (lazy SSnil) -> lazy SSnil
  | (lazy (SScons(x, ss))) -> x ++ (concat ss)

let concatMap f s = concat (map f s)

let maximum s =
  let max a b = if a > b then a else b
  in foldl1 max s

let minimum s =
  let min a b = if a < b then a else b
  in foldl1 min s

(* *** Building lists *)
(* *** scan *)
let rec scanl f q ls =
  lazy (SScons(q, match ls with
    | (lazy SSnil)           -> lazy SSnil
    | (lazy (SScons(x, xs))) -> scanl f (f q x) xs))

let scanl1 f s = match s with
  | (lazy (SScons(x, s))) -> scanl f x s
  | (lazy SSnil)          -> lazy SSnil

let rec scanr f q s = lazy(match s with
  | (lazy SSnil)           -> SScons(q, lazy SSnil)
  | (lazy (SScons(x, xs))) -> let qs = scanr f q xs
			      in SScons(f x (head qs), qs))

let rec scanr1 f s = lazy(match s with
  | (lazy SSnil)                   -> SSnil
  | (lazy (SScons(x, lazy SSnil))) -> SScons(x, lazy SSnil)
  | (lazy (SScons(x, xs)))         -> let qs = scanr1 f xs
				      in SScons(f x (head qs), qs))

(* *** Sublists *)
let rec take n s = lazy (match (n, s) with
  | (0, _) -> SSnil
  | (_, lazy SSnil) -> SSnil
  | (n, lazy (SScons(x, s))) -> SScons(x, take (n - 1) s))

let drop n s = lazy (
  let rec drop' n s = match (n, s) with
    | (0, lazy s) -> s
    | (_, lazy SSnil) -> SSnil
    | (n, lazy (SScons(_, s))) -> drop' (n - 1) s
  in drop' n s)

let splitAt n xs = (take n xs, drop n xs)

let rec takeWhile p s = lazy(match s with
  | (lazy SSnil)           -> SSnil
  | (lazy (SScons(x, xs))) ->
    if p x then SScons(x, takeWhile p xs) else SSnil)

let rec dropWhile p s = match s with
  | (lazy SSnil)                -> SSnil
  | (lazy (SScons(x, xs) as s)) -> if p x then dropWhile p xs else s

let rec span p s = match s with
  | ((lazy SSnil) as e)    -> (e, e)
  | (lazy (SScons(x, xs))) ->
    if p x then let (ys, zs) = span p xs in (lazy (SScons(x, ys)), zs)
    else (lazy SSnil, xs)

let ($) f g x = f (g x) (* like Haskel's (.) *)
let break p = span (not $ p)

(* *** Infinite lists *)
let rec iterate f x = lazy (SScons(x, iterate f (f x)))

let repeat x = let rec xs = lazy (SScons(x, xs))
	       in xs

let replicate n x = take n (repeat x)

let rec cycle s = match s with
  | (lazy SSnil) -> raise Empty_stream
  | s            -> s ++ cycle s

(* *** Searching lists *)

(* elem *)
(* notElem *)
(* lookup *)

(* *** Zipping and unzipping lists *)

(* zip *)
(* zip3 *)
(* zipWith *)
(* zipWith3 *)
(* unzip *)
(* unzip3 *)

(* *** Functions on strings *)

(* lines *)
(* words *)
(* unlines *)
(* unwords *)

(*
http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html
*)
(* *** Unfolding *)

(* unfoldr *)

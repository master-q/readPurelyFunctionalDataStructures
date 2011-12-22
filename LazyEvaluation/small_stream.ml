(* http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html *)

type 'a cell = SSnil | SScons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

(* *** List operations *)

(* map *)

let (++) t1 t2 =
  let rec pp t1 t2 = match t1 with
    | (lazy SSnil) -> t2
    | (lazy (SScons(x, s))) -> lazy (SScons(x, pp s t2))
  in lazy (pp t1 t2)

(* filter *)
(* head *)
(* last *)
(* tail *)
(* init *)
(* null *)
(* length *)
(* !! *)

let reverse s =
  let rec reverse' = function
    | (lazy SSnil, r) -> r
    | (lazy (SScons(x, s)), r) -> reverse' (s, lazy (SScons(x, r)))
  in lazy (reverse' (s, lazy SSnil))

(* *** Reducing lists (folds) *)

let foldr f z s =
  let rec foldr' f z s = match (f, z, s) with
    | (_, z, lazy SSnil) -> z
    | (f, z, lazy (SScons(x, s))) -> f x (foldr' f z s)
  in lazy (foldr' f z s)

(* foldr1 *)

(* *** Special folds *)

(* and *)
(* or *)
(* any *)
(* all *)
(* sum *)
(* product *)
(* concat *)
(* concatMap *)
(* maximum *)
(* minimum *)

(* *** Building lists *)
(* *** scan *)

(* scanl *)
(* scanl1 *)
(* scanr *)
(* scanr1 *)

(* *** Infinite lists *)

(* iterate *)
let repeat x = let rec xs = lazy (SScons(x, xs))
	       in xs

(* replicate *)
(* cycle *)

(* *** Sublists *)

let take n s =
  let rec take' n' s' = match (n', s') with
    | (0, _) -> lazy SSnil
    | (_, lazy SSnil) -> lazy SSnil
    | (n'', lazy (SScons(x, s''))) -> lazy (SScons(x, take' (n'' - 1) s''))
  in lazy (take' n s)

let drop n s = lazy (
  let rec drop' n s = match (n, s) with
    | (0, s) -> s
    | (_, lazy SSnil) -> lazy SSnil
    | (n, lazy (SScons(_, s))) -> drop' (n - 1) s
  in drop' n s)

(* splitAt *)
(* takeWhile *)
(* dropWhile *)
(* span *)
(* break *)

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

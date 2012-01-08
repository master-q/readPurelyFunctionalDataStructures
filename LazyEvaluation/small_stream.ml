(* http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html *)

type 'a cell = SSnil | SScons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

(* *** List operations *)

(* map *)

let rec (++) t1 t2 = lazy (match (t1, t2) with
  | (lazy SSnil, lazy t2) -> t2
  | (lazy (SScons(x, s)), t2) -> SScons(x, s ++ t2))

(* filter *)
(* head *)
(* last *)
(* tail *)
(* init *)
(* null *)
(* length *)
(* !! *)

let reverse s = lazy (
  let rec reverse' s r = match (s, r) with
    | (lazy SSnil, r) -> r
    | (lazy (SScons(x, s)), r) -> reverse' s (SScons(x, lazy r))
  in reverse' s SSnil)

(* *** Reducing lists (folds) *)

let foldr f z s =
  let rec foldr' f z s = match (f, z, s) with
    | (_, z, lazy SSnil) -> z
    | (f, z, lazy (SScons(x, s))) -> f x (foldr' f z s)
  in foldr' f z s

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

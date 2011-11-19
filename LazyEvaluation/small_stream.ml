type 'a cell = SSnil | SScons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

(* let hoge = lazy (SScons(2, lazy (SScons(1, lazy SSnil)))) *)

let (++) t1 t2 =
  let rec pp t1 t2 = match t1 with
    | (lazy SSnil) -> t2
    | (lazy (SScons(x, s))) -> lazy (SScons(x, pp s t2))
  in lazy (pp t1 t2)
let take n s =
  let rec take' n' s' = match (n', s') with
    | (0, _) -> lazy SSnil
    | (_, lazy SSnil) -> lazy SSnil
    | (n'', lazy (SScons(x, s''))) -> lazy (SScons(x, take' (n'' - 1) s''))
  in lazy (take' n s)
let drop n s =
  let rec drop' n' s' = match (n', s') with
    | (0, s'') -> s''
    | (_, lazy SSnil) -> lazy SSnil
    | (n'', lazy (SScons(_, s'''))) -> drop' (n'' - 1) s
  in lazy (drop' n s)
let reverse s =
  let rec reverse' = function
    | (lazy SSnil, r) -> r
    | (lazy (SScons(x, s)), r) -> reverse' (s, lazy (SScons(x, r)))
  in lazy (reverse' (s, lazy SSnil))

open List

module BinomialHeap = struct
  type 'a tree = Node of int * 'a * ('a tree) list
  type 'a heap = ('a tree) list

  let empty = []
  let isEmpty ts = ts = []
  let rank = function
    | Node(r, _, _) -> r
  let root = function
    | Node(_, x, _) -> x
  let link (Node(r, x1, c1) as t1) (Node(_, x2, c2) as t2) =
    if x1 <= x2 then Node(r + 1, x1, t2 :: c1)
    else Node(r + 1, x2, t1 :: c2)
  let rec insTree t = function
    | [] -> [t]
    | (t' :: ts') as ts ->
      if rank t < rank t' then t :: ts else insTree (link t t') ts'
  let insert x ts = insTree (Node(0, x, [])) ts
  let rec insertAll a1 a2 = match (a1, a2) with
    | ([], ts) -> ts
    | (x :: xs, ts) -> insertAll xs (insert x ts)
  let rec merge a1 a2 = match (a1, a2) with
    | (ts, []) -> ts
    | ([], ts) -> ts
    | (((t1 :: ts1') as ts1), ((t2 :: ts2') as ts2)) ->
      if rank t1 < rank t2 then t1 :: merge ts1' ts2
      else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
      else insTree (link t1 t2) (merge ts1' ts2')
  let rec removeMinTree = function
    | [] -> raise (Failure "removeMinTree: don't call with null list.")
    | [t] -> (t, [])
    | t :: ts ->
      let (t', ts') = removeMinTree ts
      in if root t <= root t'
	then (t, ts) else (t', t :: ts')
  let findMin ts = let (t, _) = removeMinTree ts in root t
  let deleteMin ts =
    let (Node(_, x, ts1), ts2) = removeMinTree ts
    in merge (rev ts1) ts2
end

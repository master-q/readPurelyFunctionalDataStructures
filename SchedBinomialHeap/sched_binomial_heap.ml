open List
open Printf
open Small_stream

exception SchedBinomialHeap_empty

module SBHeap = struct
  type 'a tree = Node of 'a * ('a tree) list
  type 'a digit = Zero | One of 'a tree
  type 'a schedule = 'a digit stream list
  type 'a heap = 'a digit stream * 'a schedule

  let empty = (lazy SSnil, [])
  let isEmpty = function
    | (lazy SSnil, _) -> true
    | _               -> false
  let link ((Node(x1, c1) as t1), (Node(x2, c2) as t2)) =
    if x1 <= x2 then Node(x1, t2 :: c1)
    else Node(x2, t1 :: c2)
  let rec insTree = function
    | (t, lazy SSnil)                -> lazy (SScons(One t, lazy SSnil))
    | (t, lazy (SScons(Zero, ds)))   -> lazy (SScons(One t, lazy SSnil))
    | (t, lazy (SScons(One t', ds)))
      -> lazy (SScons(Zero, insTree(link (t, t'), ds)))
  let rec mrg = function
    | (ds1, lazy SSnil) -> ds1
    | (lazy SSnil, ds2) -> ds2
    | (lazy (SScons(Zero, ds1)), lazy (SScons(d, ds2)))
      -> lazy (SScons(d, mrg (ds1, ds2)))
    | (lazy (SScons(d, ds1)), lazy (SScons(Zero, ds2)))
      -> lazy (SScons(d, mrg (ds1, ds2)))
    | (lazy (SScons(One t1, ds1)), lazy (SScons(One t2, ds2)))
      -> lazy (SScons(Zero, insTree (link (t1, t2), mrg (ds1, ds2))))
  let rec normalize = function
    | ((lazy SSnil) as ds)            -> ds
    | ((lazy (SScons(_, ds'))) as ds) ->
      let nop a = () in (nop (normalize ds'); ds)
  let exec = function
    | [] -> []
    | ((lazy (SScons(Zero, job))) :: sched) -> job :: sched
    | (_ :: sched) -> sched
  let insert (x, (ds, sched)) =
    let ds' = insTree (Node(x, []), ds)
    in (ds', exec (exec (ds' :: sched)))
  let merge ((ds1, _), (ds2, _)) =
    let ds = normalize (mrg (ds1, ds2)) in (ds, [])
  let rec removeMinTree = function
    | lazy SSnil -> raise SchedBinomialHeap_empty
    | lazy (SScons(One t, lazy SSnil)) -> (t, lazy SSnil)
    | lazy (SScons(Zero, ds))
      -> let (t', ds') = removeMinTree ds
	 in (t', lazy (SScons(Zero, ds')))
    | lazy (SScons(One (Node(x, _) as t), ds))
      -> match removeMinTree ds with
      | (Node(x', _) as t', ds')
	-> if x <= x' then (t, lazy (SScons(Zero, ds)))
	  else (t', lazy (SScons(One t, ds')))
  let findMin (ds, _) =
    let (Node(x, _), _) = removeMinTree ds in x
  let deleteMin (ds, _) =
    let (Node(x, c), ds') = removeMinTree ds in
    let ds'' = mrg (listToStream (lmap One (rev c), ds')) in
    (normalize ds'', [])
end

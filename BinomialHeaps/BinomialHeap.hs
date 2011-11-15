module BinomialHeap where

data Tree a = Node Int a [Tree a]
type Heap a = [Tree a]

link :: Ord a => Tree a -> Tree a -> Tree a
link (t1@(Node r x1 c1)) (t2@(Node _ x2 c2)) =
  if x1 < x2
    then Node (r + 1) x1 (t2:c1)
    else Node (r + 1) x2 (t1:c2)

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ts@(t':ts') = if rank t < rank t' then t:ts
                          else insTree (link t t') ts'

insert :: Ord a => a -> Heap a -> Heap a
insert x = insTree (Node 0 x [])

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts [] = ts
merge [] ts = ts
merge ts1@(t1:ts1') ts2@(t2:ts2') =
  if rank t1 < rank t2 then t1:merge ts1' ts2
    else if rank t2 < rank t1 then t2:merge ts1 ts2'
           else insTree (link t1 t2) (merge ts1' ts2')

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
  let (t', ts') = removeMinTree ts
  in if root t < root t' then (t, ts) else (t', t:ts')

findMin :: Ord a => Heap a -> a
findMin ts = let (t, _) = removeMinTree ts in root t

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts =
  let (Node _ x ts1, ts2) = removeMinTree ts
  in merge (reverse ts1) ts2

module BatchedQueue (BatchedQueue) where
import Prelude hiding (head,tail)
import Queue

data BatchedQueue a = BQ [a] [a]

check :: [a] -> [a] -> BatchedQueue a
check [] r = BQ (reverse r) []
check f r = BQ f r

flatten :: BatchedQueue a -> [a]
flatten (BQ f r) = f ++ reverse r

instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f _) = null f
  snoc (BQ f r) x = check f (x:r)
  head (BQ [] _) = error "empty queue"
  head (BQ (x:_) _) = x
  tail (BQ [] _) = error "empty queue"
  tail (BQ (_:f) r) = check f r
  (===) _ _ = error "hoge"
  {--
  (===) a b = go (flatten a) (flatten b)
    where go :: [a] -> [a] -> Bool
          go x y = and $ zipWith (==) x y
--}

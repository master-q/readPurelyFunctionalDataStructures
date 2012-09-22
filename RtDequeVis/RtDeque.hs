module RtDeque (dqEmpty, dqIsEmpty, dqCons, dqHead, dqTail, dqSnoc) where

data RtDq a = RtDq Int [a] [a] Int [a] [a]
            deriving Show

c :: Int
c = 2 -- or 3

dqEmpty :: RtDq a
dqEmpty = RtDq 0 [] [] 0 [] []

dqIsEmpty :: RtDq a -> Bool
dqIsEmpty (RtDq lenf _ _ lenr _ _) = lenf + lenr == 0

exec1 :: [a] -> [a]
exec1 (_:xs) = xs
exec1 xs = xs

exec2 :: [a] -> [a]
exec2 = exec1 . exec1

rotateRev :: [a] -> [a] -> [a] -> [a]
rotateRev [] r a = reverse r ++ a
rotateRev (x:f) r a =
  x : rotateRev f (drop c r) (reverse (take c r) ++ a)

rotateDrop :: [a] -> Int -> [a] -> [a]
rotateDrop f j r =
  if j < c then rotateRev f (drop j r) []
  else let (x:f') = f
       in x : rotateDrop f' (j - c) (drop c r)

check :: RtDq a -> RtDq a
check q@(RtDq lenf f _ lenr r _) =
  if lenf > c * lenr + 1 then
    let i = (lenf + lenr) `div` 2
        j = lenf + lenr - i
        f' = take i f
        r' = rotateDrop r i f
    in RtDq i f' f' j r' r'
  else if lenr > c * lenf + 1 then
         let j = (lenf + lenr) `div` 2
             i = lenf + lenr - j
             r' = take j r
             f' = rotateDrop f j r
         in RtDq i f' f' j r' r'
       else q

dqCons :: a -> RtDq a -> RtDq a
dqCons x (RtDq lenf f sf lenr r sr) =
  check (RtDq (lenf + 1) (x:f) (exec1 sf) lenr r (exec1 sr))

dqHead :: RtDq a -> a
dqHead (RtDq _ [] _ _ [] _) = error "*** dqHead: Empty"
dqHead (RtDq _ [] _ _ (x:_) _) = x
dqHead (RtDq _ (x:_) _ _ _ _) = x

dqTail :: RtDq a -> RtDq a
dqTail (RtDq _ [] _ _ [] _) = error "*** dqTail: Empty"
dqTail (RtDq _ [] _ _ (_:_) _) = dqEmpty
dqTail (RtDq lenf (_:f') sf lenr r sr) =
  check (RtDq (lenf - 1) f' (exec2 sf) lenr r (exec2 sr))

dqSnoc :: RtDq a -> a -> RtDq a
dqSnoc (RtDq lenf f sf lenr r sr) x =
  check (RtDq lenf f (exec1 sf) (lenr + 1) (x:r) (exec1 sr))

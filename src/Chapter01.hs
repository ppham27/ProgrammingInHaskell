module Chapter01 where

double x = x + x

product' xs = foldr (*) 1 xs

qsort :: (Ord x) => [x] -> [x]
qsort = qsortBy (<=)

qsortReverse :: (Ord x) => [x] -> [x]
qsortReverse = qsortBy (>)

qsortBy :: (Ord x) => (x -> x -> Bool) -> [x] -> [x]
qsortBy f [] = []
qsortBy f (x:xs) = qsortBy f smaller ++ [x] ++ qsortBy f larger
  where
    smaller = [a | a <- xs,  a `f` x]
    larger = [b | b <- xs, not (b `f` x)]

module Chapter01 where

product' [] = 1
product' (x:xs) = x * product' xs

qsort :: (Ord x) => [x] -> [x]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- qsortReverse :: (Ord x) => [x] -> x[x]

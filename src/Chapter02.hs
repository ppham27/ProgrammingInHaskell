module Chapter02 where

last' :: [x] -> x
last' xs = case xs of
  [] -> error "empty list"  
  xs -> xs !! (length xs - 1)

init' :: [x] -> [x]
init' xs = case xs of
  [] -> error "empty list"
  xs -> take (length xs - 1) xs

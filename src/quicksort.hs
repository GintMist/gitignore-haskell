module Quicksort where

quicksort [] = []
quicksort (x:xs) = smallers ++ (x : biggers)
  where
    smallers = quicksort $ filter (< x) xs
    biggers = quicksort $ filter (>= x) xs

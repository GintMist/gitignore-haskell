module Present where

import           Data.List   (nub)
import           Quicksort
import           Text.Printf

presentOptions :: [String] -> IO [String]
presentOptions xs = do
  putStrLn "Please choose one of the followings:\n"
  pr opts 0
  a <- getLine
  if all (< length opts) (read <$> words a :: [Int])
  then  return $ fmap (opts !!) (read <$> words a :: [Int])
  else return []
  where
    pr :: [String] -> Int -> IO ()
    pr [] _ = putStrLn "\nPlease enter an index:"
    pr (o:os) n = (putStrLn $ printf "[%d] %s" n o) >> pr os (n + 1)
    opts = quicksort $ nub xs

module Present where

import           Quicksort
import           Text.Printf

presentOptions :: [String] -> IO [String]
presentOptions xs = do
  putStrLn "Please choose from followings, you can choose more than one they will be merged:\n"
  pr opts 0
  a <- getLine
  if all (< length opts) (read <$> words a :: [Int])
  then  return $ fmap (opts !!) (read <$> words a :: [Int])
  else return []
  where
    pr :: [String] -> Int -> IO ()
    pr [] _ = putStrLn "\nPlease enter index(es):"
    pr (o:os) n = putStrLn (printf "[%d] %s" n o) >> pr os (n + 1)
    opts = quicksort xs

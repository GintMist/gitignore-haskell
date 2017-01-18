module Present where

import           Text.Printf

presentOptions :: [String] -> IO [String]
presentOptions opts = do
  putStrLn "Please choose one of the followings:\n"
  pr opts 0
  index <- getLine
  if ((read index) :: Int) < length opts
  then  return [opts !! read index]
  else return []
  where
    pr :: [String] -> Int -> IO ()
    pr [] _ = putStrLn "\nPlease enter an index:"
    pr (o:os) n = (putStrLn $ printf "[%d] %s" n o) >> pr os (n + 1)

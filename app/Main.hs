module Main where

import           Gitignore
import           IgnoreFiles
import           Present
import           System.Environment (getArgs)

main :: IO ()
main = do
  arg <- getArgs
  if not $ null arg
  then let arg' = fmap normalize arg
           argFile = (\p -> filter (=.= p) ignoreFiles) =<< arg'
       in if all (`elem` ignoreFiles) argFile
          then writeNewIgnoreFile argFile
          else putStrLn "Invalid argument"
  else do
    allGuesses <- getAllEnvironmentGuesses
    if not $ null allGuesses
    then do
      opt <- presentOptions allGuesses
      if not $ null opt
      then writeNewIgnoreFile opt
      else putStrLn "Wrong index"
    else putStrLn "Sorry, couldn't find any .gitignore file."

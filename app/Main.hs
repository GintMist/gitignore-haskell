module Main where

import           Gitignore
import           IgnoreFiles
import           Present
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

main :: IO ()
main = do
  cd <- getCurrentDirectory
  arg <- getArgs
  if not $ null arg
  then let arg' = fmap normalize arg
           argFile = (\p -> filter (=.= p) ignoreFiles) =<< arg'
       in if all (`elem` ignoreFiles) argFile
          then writeNewIgnoreFile cd argFile
          else putStrLn "Invalid argument"
  else do
    allGuesses <- getAllEnvironmentGuesses cd
    if not $ null allGuesses
    then do
      opt <- presentOptions allGuesses
      if not $ null opt
      then writeNewIgnoreFile cd opt
      else putStrLn "Wrong index"
    else putStrLn "Sorry, couldn't find any .gitignore file."

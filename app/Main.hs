module Main where

import           Gitignore
import           IgnoreFiles
import           Present
import           System.Environment (getArgs)

main :: IO ()
main = do
  arg <- getArgs
  parentGuess <- guessFromParentFolder
  extensionGuess <- guessFromFileExtensions
  if not $ null arg
  then let argFile = normalize (head arg)
       in if argFile `elem` ignoreFiles
          then writeNewIgnoreFile argFile
          else putStrLn "Invalid argument"
  else if not $ null parentGuess
       then writeNewIgnoreFile (head parentGuess)
       else if not $ null extensionGuess
            then do
              opt <- presentOptions extensionGuess
              if not $ null opt
              then writeNewIgnoreFile (head opt)
              else putStrLn "Wrong index"
            else putStrLn "Sorry, couldn't find any .gitignore file."

module Gitignore ( writeNewIgnoreFile
                 , guessFromParentFolder
                 , guessFromFileExtensions
                 , normalize
                 , (=.=)
                 ) where

import           Control.Lens         ((^.))
import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as BL (ByteString, concat, writeFile)
import           Data.Char            (toLower)
import           Data.List            (intersect, nub)
import           IgnoreFiles
import           Network.Wreq         (get, responseBody)
import           System.Directory     (doesFileExist, getCurrentDirectory,
                                       listDirectory, renameFile)
import           System.FilePath      (takeBaseName, takeDirectory,
                                       takeExtension, (<.>))
backupOldGitignore :: IO ()
backupOldGitignore = do
  existence <- doesFileExist ".gitignore"
  when existence $ renameFile ".gitignore" ".gitignore.old"

getIgnoreFile :: String -> IO BL.ByteString
getIgnoreFile = fmap (^. responseBody) . get . (baseURL ++)
  where
    baseURL = "https://raw.githubusercontent.com/github/gitignore/master/"

writeNewIgnoreFile :: [String] -> IO ()
writeNewIgnoreFile nif = do
  backupOldGitignore
  newFile <- sequenceA $ fmap getIgnoreFile (nub nif)
  BL.writeFile ".gitignore" (BL.concat newFile)
  putStrLn "New .gitignore file has been written"

getParentFolderName :: IO String
getParentFolderName = fmap (takeBaseName . takeDirectory) getCurrentDirectory

normalize :: String -> String
normalize = (<.> "gitignore")

(=.=) :: String -> String -> Bool
[] =.= [] = True
[] =.= _  = False
_ =.= []  = False
(x:xs) =.= (y:ys) = (toLower x == toLower y) && (xs =.= ys)

guessFromParentFolder :: IO [String]
guessFromParentFolder = (normalize <$> getParentFolderName) >>=
                        \p -> return $ filter (=.= p) ignoreFiles

getAllFileExtensions :: IO [String]
getAllFileExtensions = do
  currentDir <- getCurrentDirectory
  content <- listDirectory currentDir
  (fmap . fmap) (tail . takeExtension) (filterFilesWithExt content [])
  where
    filterFilesWithExt [] acc = return acc
    filterFilesWithExt (c:cs) acc = do
      existence <- doesFileExist c
      if existence && '.' `elem` c && c `notElem` acc
      then filterFilesWithExt cs (c : acc)
      else filterFilesWithExt cs acc

guessFromFileExtensions :: IO [String]
guessFromFileExtensions = do
  allExt <- getAllFileExtensions
  return . concat $ fmap (\(ignore, exts) -> if null $ intersect exts allExt
                                             then []
                                             else [ignore])
                                             extensions

module Gitignore ( writeNewIgnoreFile
                 , normalize
                 , (=.=)
                 , getAllEnvironmentGuesses
                 ) where

import           Control.Lens         ((^.))
import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as BL (ByteString, concat, writeFile)
import           Data.Char            (toLower)
import           Data.List            (intersect, nub)
import           IgnoreFiles
import           Network.Wreq         (get, responseBody)
import           System.Directory     (doesDirectoryExist, doesFileExist,
                                       getCurrentDirectory, listDirectory,
                                       renameFile, setCurrentDirectory)
import           System.FilePath      (takeBaseName, takeDirectory,
                                       takeExtension, (<.>), (</>))

backupOldGitignore :: IO ()
backupOldGitignore = do
  existence <- doesFileExist ".gitignore"
  when existence $ renameFile ".gitignore" ".gitignore.old"

getIgnoreFile :: String -> IO BL.ByteString
getIgnoreFile = fmap (^. responseBody) . get . (baseURL ++)
  where
    baseURL = "https://raw.githubusercontent.com/github/gitignore/master/"

writeNewIgnoreFile :: String -> [String] -> IO ()
writeNewIgnoreFile path nif = do
  setCurrentDirectory path
  backupOldGitignore
  newFile <- sequenceA $ fmap getIgnoreFile (nub nif)
  BL.writeFile ".gitignore" (BL.concat newFile)
  putStrLn "New .gitignore file has been written"

getParentFolderName :: String -> IO String
getParentFolderName path = return $ (takeBaseName . takeDirectory) path

normalize :: String -> String
normalize = (<.> "gitignore")

(=.=) :: String -> String -> Bool
a =.= b = (toLower <$> a) == (toLower <$> b)

guessFromParentFolder :: String -> IO [String]
guessFromParentFolder path = (normalize <$> getParentFolderName path) >>=
                             \p -> return $ filter (=.= p) ignoreFiles

getSubDirectories :: String -> IO [String]
getSubDirectories path = do
  content <- listDirectory path
  go [] content
  where
    go acc [] = return acc
    go acc (x:xs) = do
      e <- doesDirectoryExist x
      if e
      then go (x : acc) xs
      else go acc xs

getFiles :: String -> IO [String]
getFiles path =  do
  content <- listDirectory path
  go [] content
  where
    go acc [] = return acc
    go acc (x:xs) = do
      e <- doesFileExist x
      if e
      then go (x : acc) xs
      else go acc xs

getAllEnvironmentGuesses :: String -> IO [String]
getAllEnvironmentGuesses path = fmap nub
                                $ (++)
                                <$> guessFromFileExtensions path
                                <*> guessFromParentFolder path

getAllFileExtensions :: String -> IO [String]
getAllFileExtensions path = do
  setCurrentDirectory path
  files <- getFiles path
  subDirectories <- (fmap . fmap) (path </>)(getSubDirectories path)
  subExts <- sequenceA (fmap getAllFileExtensions subDirectories)
  return $ (fmap tail $ filter (not . null) (fmap takeExtension files)) ++ (concat subExts)

guessFromFileExtensions :: String -> IO [String]
guessFromFileExtensions path = do
  allExt <- getAllFileExtensions path
  return . concat $ fmap (\(ignore, exts) -> if null $ intersect exts allExt
                                             then []
                                             else [ignore])
                                             extensions

module Gitignore where

import           Control.Lens         ((^.))
import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (toLower, toUpper)
import           Data.List            (intersect)
import           IgnoreFiles
import           Network.Wreq
import           System.Directory

backupOldGitignore :: IO ()
backupOldGitignore = do
  existence <- doesFileExist ".gitignore"
  when existence $ renameFile ".gitignore" ".gitignore.old"

getIgnoreFile :: String -> IO BL.ByteString
getIgnoreFile = fmap (^. responseBody) . get . (baseURL ++)
  where
    baseURL = "https://raw.githubusercontent.com/github/gitignore/master/"

writeNewIgnoreFile :: String -> IO ()
writeNewIgnoreFile nif = do
  backupOldGitignore
  newFile <- getIgnoreFile nif
  BL.writeFile ".gitignore" newFile
  putStrLn "New .gitignore file has been written"


getParentFolderName :: IO (Maybe String)
getParentFolderName = fmap (toParent "" False) getCurrentDirectory
  where
    toParent _ _ [] = Nothing
    toParent _ False directory = toParent "" (last directory == '\\') (init directory)
    toParent p True directory = if last directory == '\\'
                                then Just p
                                else toParent (last directory : p) True (init directory)

normalize :: String -> String
normalize (c:cs) = toUpper c : fmap toLower cs ++ ".gitignore"

guessFromParentFolder :: IO [String]
guessFromParentFolder = do
  parent <- getParentFolderName
  case parent of
    Nothing -> return []
    Just n  -> if normalize n `elem` ignoreFiles
               then return [normalize n]
               else return []


getAllFileExtensions :: IO [String]
getAllFileExtensions = do
  currentDir <- getCurrentDirectory
  content <- listDirectory currentDir
  (fmap . fmap) (getExtension "") (filterFilesWithExt content [])
  where
    filterFilesWithExt [] acc = return acc
    filterFilesWithExt (c:cs) acc = do
      existence <- doesFileExist c
      if existence && '.' `elem` c && c `notElem` acc
      then filterFilesWithExt cs (c : acc)
      else filterFilesWithExt cs acc
    getExtension p filename
      | last filename == '.' = p
      | otherwise = getExtension (toLower (last filename) : p) (init filename)

guessFromFileExtensions :: IO [String]
guessFromFileExtensions = do
  allExt <- getAllFileExtensions
  return . concat $ fmap (\(ignore, exts) -> if null $ intersect exts allExt
                                                         then []
                                                         else [ignore])
                                                         extensions

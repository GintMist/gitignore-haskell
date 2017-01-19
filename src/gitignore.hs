module Gitignore ( writeNewIgnoreFile
                 , normalize
                 , (=.=)
                 , getAllEnvironmentGuesses
                 ) where

import           Control.Lens         ((^.))
import           Control.Monad        (filterM, join, when)
import qualified Data.ByteString.Lazy as BL (ByteString, concat, writeFile)
import           Data.Char            (toLower)
import           Data.List            (intersect, nub)
import           IgnoreFiles
import           Network.Wreq         (get, responseBody)
import           System.Directory     (doesDirectoryExist, doesFileExist,
                                       getCurrentDirectory, listDirectory,
                                       renameFile)
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

writeNewIgnoreFile :: [String] -> IO ()
writeNewIgnoreFile nif = do
  backupOldGitignore
  newFile <- sequenceA $ fmap getIgnoreFile (nub nif)
  BL.writeFile ".gitignore" (BL.concat newFile)
  putStrLn "New .gitignore file has been written"

getParentFolderName :: IO String
getParentFolderName = (takeBaseName . takeDirectory) <$> getCurrentDirectory

normalize :: String -> String
normalize = (<.> "gitignore")

(=.=) :: String -> String -> Bool
a =.= b = (toLower <$> a) == (toLower <$> b)

guessFromParentFolder :: IO [String]
guessFromParentFolder = (normalize <$> getParentFolderName) >>=
                        \p -> return $ filter (=.= p) ignoreFiles

getSubDirectories :: String -> IO [String]
getSubDirectories path = listDirectory path >>= (filterM doesDirectoryExist . fmap (path </>))

getFiles :: String -> IO [String]
getFiles path = listDirectory path >>= filterM (doesFileExist . (path </>))

getAllFiles :: String -> IO [String]
getAllFiles path = (++) <$> (getFiles path)
                        <*> ((getSubDirectories path)
                        >>= ((fmap concat) . (traverse getAllFiles)))

getAllFileExtensions :: String -> IO [String]
getAllFileExtensions = (fmap (filter (not . null) . (fmap takeExtension))) . getAllFiles

guessFromFileExtensions :: IO [String]
guessFromFileExtensions = do
  path <- getCurrentDirectory
  allExt <- (fmap.fmap) tail (getAllFileExtensions path)
  return . concat $ fmap (\(ignore, exts) -> if null $ intersect exts allExt
                                             then []
                                             else [ignore])
                                             extensions

getAllEnvironmentGuesses :: IO [String]
getAllEnvironmentGuesses = fmap nub
                           $ (++)
                           <$> guessFromFileExtensions
                           <*> guessFromParentFolder

module Directory.Tree where

import Control.Monad
import qualified Data.Text.IO as TIO 
import System.Directory
import System.FilePath

import Directory.Directory

tree :: FilePath -> IO Directory
tree path = makeDirectoryRelative <$> (treeAbsolute path)


makeDirectoryRelative :: Directory -> Directory
makeDirectoryRelative (Directory path directories files) = Directory 
                                                    (takeFileName path) 
                                                    (map makeDirectoryRelative directories) 
                                                    (map makeFileRelative files)


makeFileRelative :: File -> File
makeFileRelative (File path content) = File (takeFileName path) content


treeAbsolute :: FilePath -> IO Directory
treeAbsolute path = Directory path <$> (parseDirectories path) <*> (parseFiles path)


parseFile :: FilePath -> IO File
parseFile path = File path <$> TIO.readFile path


parseDirectoryEntries :: (FilePath -> IO Bool) -> (FilePath -> IO a) -> FilePath -> IO [a] 
parseDirectoryEntries filterer parser path = getAbsoluteDirectoryContentsStrict path
                                             >>= filterM filterer
                                             >>= mapM parser


parseFiles :: FilePath -> IO [File]
parseFiles = parseDirectoryEntries doesFileExist parseFile


parseDirectories :: FilePath -> IO [Directory]
parseDirectories = parseDirectoryEntries doesDirectoryExist treeAbsolute


notIn :: (Eq a, Foldable t) => t a -> a -> Bool
notIn xs x = not $ x `elem` xs


getDirectoryContentsStrict :: FilePath -> IO [FilePath]
getDirectoryContentsStrict path = (filter $ notIn [".", ".."]) <$> getDirectoryContents path


prepend :: FilePath -> FilePath -> FilePath
prepend p1 p2 = p1 ++ "/" ++ p2


getAbsoluteDirectoryContentsStrict :: FilePath -> IO [FilePath]
getAbsoluteDirectoryContentsStrict path = (map $ prepend path) <$> getDirectoryContentsStrict path

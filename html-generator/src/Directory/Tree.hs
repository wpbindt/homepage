module Directory.Tree where

import Control.Monad
import qualified Data.Text.IO as TIO 
import System.Directory

import Directory.Directory


tree :: FilePath -> IO Directory
tree path = Directory path <$> (parseDirectories path) <*> (parseFiles path)


parseFile :: FilePath -> IO File
parseFile path = File path <$> TIO.readFile path


parseDirectoryEntries :: (FilePath -> IO Bool) -> (FilePath -> IO a) -> FilePath -> IO [a] 
parseDirectoryEntries filterer parser path = (getAbsoluteDirectoryContentsStrict path) >>= (filterM filterer) >>= (mapM parser)


parseFiles :: FilePath -> IO [File]
parseFiles = parseDirectoryEntries doesFileExist parseFile


parseDirectories :: FilePath -> IO [Directory]
parseDirectories = parseDirectoryEntries doesDirectoryExist tree


notIn :: (Eq a, Foldable t) => t a -> a -> Bool
notIn xs x = not $ x `elem` xs


getDirectoryContentsStrict :: FilePath -> IO [FilePath]
getDirectoryContentsStrict = fmap (filter $ notIn [".", ".."]) . getDirectoryContents


prepend :: FilePath -> FilePath -> FilePath
prepend p1 p2 = p1 ++ "/" ++ p2


getAbsoluteDirectoryContentsStrict :: FilePath -> IO [FilePath]
getAbsoluteDirectoryContentsStrict path = fmap (map (prepend path)) $ getDirectoryContentsStrict path

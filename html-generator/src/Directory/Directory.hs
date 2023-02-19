module Directory.Directory where

import qualified Data.Text as T
import System.FilePath


data File = File
    { getFileName :: FilePath
    , getContent :: T.Text
    } deriving (Show, Eq)

data Directory = Directory
    { getDirName :: FilePath
    , getDirectories :: [Directory]
    , getFiles :: [File]
    } deriving (Show, Eq)


filterByExtension :: String -> Directory -> Directory
filterByExtension extension (Directory dirName directories files) = Directory dirName filteredDirs filteredFiles
    where filteredFiles = filterFilesByExtension extension files
          filteredDirs = map (filterByExtension extension) directories


filterFilesByExtension :: String -> [File] -> [File]
filterFilesByExtension extension = filter $ isExtensionOf extension . getFileName


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x


findByExtension :: String -> Directory -> Maybe File
findByExtension extension (Directory _ _ files) = maybeHead . filterFilesByExtension extension $ files


addFile :: Directory -> File -> Directory
addFile (Directory dirName directories files) file = Directory dirName directories (file:files)


maybeAddFile :: Directory -> Maybe File -> Directory
maybeAddFile directory Nothing = directory
maybeAddFile directory (Just file) = addFile directory file

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
    where filteredFiles = filter (isExtensionOf extension . getFileName) files
          filteredDirs = map (filterByExtension extension) directories

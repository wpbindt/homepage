module Directory.Directory where

import qualified Data.Text as T


data File = File
    { getFileName :: FilePath
    , getContent :: T.Text
    } deriving (Show, Eq)

data Directory = Directory
    { getDirName :: FilePath
    , getDirectories :: [Directory]
    , getFiles :: [File]
    } deriving (Show, Eq)

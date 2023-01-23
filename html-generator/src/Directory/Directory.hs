module Directory.Directory where

import qualified Data.Text as T


data File = File FilePath T.Text deriving (Show, Eq)
data Directory = Directory FilePath [Directory] [File] deriving (Show, Eq)

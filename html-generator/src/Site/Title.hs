module Site.Title where

import qualified Data.Text as T
import System.FilePath


type Title = [T.Text]


toFileName :: Title -> String -> FilePath
toFileName title extension = addExtension hyphenatedTitle extension
        where hyphenatedTitle = T.unpack . T.intercalate "-" $ title


toHeader :: Title -> T.Text
toHeader [] = ""
toHeader (first:remainder) = T.intercalate " " $ (T.toTitle first):remainder


toDirName :: Title -> FilePath
toDirName title = toFileName title ""

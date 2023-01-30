module Site.ParseSite where

import qualified Data.Text as T

import Directory.Directory
import Site.Site
import Site.Title


type ContentParser a = T.Text -> a


parseSite :: ContentParser a -> Directory -> Site a
parseSite contentParser (Directory dirName subdirectories _) = Site 
        (fromFileName dirName)
        $ map (parseSection contentParser) subdirectories


parseSection :: ContentParser a -> Directory -> Section a
parseSection contentParser (Directory dirName _ files) = Section 
        (fromFileName dirName)
        $ map (parsePage contentParser) files


parsePage :: ContentParser a -> File -> Page a
parsePage contentParser (File name content) = Page (fromFileName name) (contentParser content)

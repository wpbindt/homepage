module HtmlGenerator.Markup where

import qualified Data.Text as T
import qualified HtmlGenerator.MarkupParsers as New


parse :: String -> New.Document
parse input = New.parseMarkup . T.pack $ input ++ "\n"

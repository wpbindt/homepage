module Markup.Markup where

import qualified Data.Text as T


data Document = Document [Paragraph] deriving (Show, Eq)

data Paragraph = Paragraph [MarkupToken] deriving (Show, Eq)

data MarkupToken = NormalText T.Text
                   | Header Int T.Text
                   | CodeBlock [T.Text]
                   | OrderedList [T.Text]
                   | InLineCode T.Text
                   deriving (Eq, Show)

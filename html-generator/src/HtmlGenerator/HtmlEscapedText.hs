module HtmlGenerator.HtmlEscapedText
    ( escape
    , printEscaped
    , HTMLEscapedText
    ) where

import Control.Applicative
import qualified Data.Text as T

import Data.Attoparsec.Text


data HTMLEscapedText = HTMLEscapedText { printEscaped :: T.Text } deriving (Show, Eq)


to :: Char -> T.Text -> Parser T.Text
c `to` s = s <$ char c


escapeHTMLChar :: Parser T.Text
escapeHTMLChar = '<' `to` "&lt;"
                 <|> '>' `to` "&gt;"
                 <|> '\"' `to` "&quot;"
                 <|> '&' `to` "&amp;"
                 <|> '\'' `to` "&#39;"
                 <|> T.singleton <$> satisfy (notInClass "<>\"&\'")


escapeHTML :: Parser T.Text
escapeHTML = ((<>) <$> escapeHTMLChar <*> escapeHTML) <|> ""


escape :: T.Text -> HTMLEscapedText
escape string_ = HTMLEscapedText $ 
                     either
                         (\x -> error $ "Could not escape " <> x)
                         id  
                         (parseOnly escapeHTML string_)

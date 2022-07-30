module HtmlGenerator.HtmlEscapedText
    ( escape
    , printEscaped
    , HTMLEscapedText
    ) where

import Control.Applicative
import qualified Data.Text as T

import Data.Attoparsec.Text


data HTMLEscapedText = HTMLEscapedText { printEscaped :: T.Text } deriving (Show, Eq)


escapeHTMLChar :: Parser T.Text
escapeHTMLChar = (const "&lt;" <$> char '<')
                 <|> (const "&gt;" <$> char '>')
                 <|> (const "&quot;" <$> char '\"')
                 <|> (const "&amp;" <$> char '&')
                 <|> (const "&#39;" <$> char '\'')
                 <|> (T.singleton <$> satisfy (notInClass "<>\"&\'"))


escapeHTML :: Parser T.Text
escapeHTML = ((<>) <$> escapeHTMLChar <*> escapeHTML) <|> (pure "") 


escape :: T.Text -> HTMLEscapedText
escape string_ = HTMLEscapedText $ 
                     either
                         (\x -> error $ "Could not escape " <> x)
                         id  
                         (parseOnly escapeHTML string_)

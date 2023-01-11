module HtmlGenerator.MarkupParsers (parseMarkup) where

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import Control.Applicative
import Data.Either

import HtmlGenerator.Markup


parseMarkup :: T.Text -> Document
parseMarkup input = fromRight (Document []) (parseOnly document input)


document :: Parser Document
document = Document <$> (paragraph `sepBy` whiteSpace)


whiteSpace :: Parser ()
whiteSpace = endOfLine *> endOfLine


paragraph :: Parser Paragraph
paragraph = Paragraph <$> (markupToken `sepBy1` unImportantWhiteSpace)


markupToken :: Parser MarkupToken
markupToken = orderedList 
              <|> codeBlock 
              <|> header 
              <|> inLineCode
              <|> normalText


codeBlock :: Parser MarkupToken
codeBlock = CodeBlock <$> many1 (lineStartingWith '>')


orderedList :: Parser MarkupToken
orderedList = OrderedList <$> many1 (lineStartingWith '-')


lineStartingWith :: Char -> Parser T.Text
lineStartingWith c = T.pack
                     <$> ((option () endOfLine) *> char c 
                     *> manyTill anyChar endOfLine)


header :: Parser MarkupToken
header = Header 
         <$> headerMarker 
         <*> (T.pack <$> many1TillExclusive anyChar endOfLine)


headerMarker :: Parser Int
headerMarker = option () endOfLine *> countChar '*' <* skipSpace


countChar :: Char -> Parser Int
countChar c = length <$> many1 (char c)


inLineCode :: Parser MarkupToken
inLineCode = InLineCode . T.pack <$> ("`" *> many1TillExclusive anyChar "`" <* "`")


normalText :: Parser MarkupToken
normalText = NormalText . T.pack 
             <$> many1TillExclusive (satisfy (not . isEndOfLine)) specialChar


many1TillExclusive :: Parser a -> Parser b -> Parser [a]
many1TillExclusive p end = (:) <$> p <*> manyTill p (lookAhead end)


specialChar :: Parser ()
specialChar = endOfLine <* satisfy (inClass "*->")
              <|> (() <$ char '`')
              <|> endOfLine


unImportantWhiteSpace :: Parser ()
unImportantWhiteSpace = option () $ endOfLine <* lookAhead (satisfy (notInClass "\n*>-"))

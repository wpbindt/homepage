module HtmlGeneratorSpec (spec) where

import Test.Hspec
import HtmlGenerator

checkBodyConversion :: String -> String -> String -> Spec
checkBodyConversion message markup body = it message $ 
        (convert "Some title" markup) == 
                ("<html><head><title>Some title</title></head><body>" <> body <> "</body></html>")

emptyConvertSpec :: Spec
emptyConvertSpec = checkBodyConversion 
        "Converts an empty string to an empty document"
        "" ""

convertParagraphSpec :: Spec
convertParagraphSpec = checkBodyConversion "Converts a paragraph to a paragraph"
        "bla" "<p>bla\n</p>"

convertHeaderSpec :: Int -> Spec
convertHeaderSpec w = checkBodyConversion ("Check header of weight " <> wString)
        markup body
            where wString = show w
                  markup = (take w $ repeat '*') <> "bla"
                  openTag = "<h" <> wString <> ">"
                  closeTag = "</h" <> wString <> ">"
                  body = openTag <> "bla" <> closeTag

escapeCharacterSpec :: String -> String -> Spec
escapeCharacterSpec input output = checkBodyConversion "Converts a paragraph to a paragraph"
        input ("<p>" <> output <> "\n</p>")

spec :: Spec
spec = do
        convertHeaderSpec 1
        convertHeaderSpec 2
        convertHeaderSpec 3
        emptyConvertSpec
        convertParagraphSpec
        escapeCharacterSpec "filler >" "filler &gt;"
        escapeCharacterSpec "<" "&lt;"
        escapeCharacterSpec "&" "&amp;"
        escapeCharacterSpec "\"" "&quot;"
        escapeCharacterSpec "'" "&#39;"

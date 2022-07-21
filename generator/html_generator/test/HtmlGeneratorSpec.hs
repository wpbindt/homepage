module HtmlGeneratorSpec (spec) where

import Test.Hspec
import HtmlGenerator

checkConversion :: String -> String -> String -> String -> Spec
checkConversion message title markup html = it message $ (convert title markup) == html

emptyConvertSpec :: Spec
emptyConvertSpec = checkConversion "Converts an empty string to an empty document"
        "My title" ""
        "<html><head><title>My title</title></head><body></body></html>"

convertParagraphSpec :: Spec
convertParagraphSpec = checkConversion "Converts a paragraph to a paragraph"
        "My title" "bla"
        "<html><head><title>My title</title></head><body><p>bla\n</p></body></html>"

convertHeaderSpec :: Int -> Spec
convertHeaderSpec w = checkConversion ("Check header of weight " <> wString)
        "Some title" markup html
            where wString = show w
                  markup = (take w $ repeat '*') <> "bla"
                  openTag = "<h" <> wString <> ">"
                  closeTag = "</h" <> wString <> ">"
                  html = "<html><head><title>Some title</title></head><body>"
                    <> openTag <> "bla" <> closeTag
                    <> "</body></html>"

escapeCharacterSpec :: String -> String -> Spec
escapeCharacterSpec input output = checkConversion "Converts a paragraph to a paragraph"
        "My title" input
        ("<html><head><title>My title</title></head><body><p>" <> output <> "\n</p></body></html>")

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

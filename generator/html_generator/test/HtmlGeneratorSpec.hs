module HtmlGeneratorSpec (spec) where

import Test.Hspec
import HtmlGenerator

checkBodyConversion :: String -> String -> String -> Spec
checkBodyConversion message markup body = it message $ 
        (convert "Some title" markup) == 
                ("<html><head><title>Some title</title></head><body>" <> body <> "</body></html>")

newlineConvertSpec :: Int -> Spec
newlineConvertSpec n = checkBodyConversion
        ("Converts " <> (show n) <> " newlines to an empty document")
        (take n $ repeat '\n') ""

convertParagraphSpec :: Spec
convertParagraphSpec = checkBodyConversion 
        "Converts a paragraph to a paragraph"
        "bla" "<p>bla\n</p>"

convertMultipleParagraphSpec :: Spec
convertMultipleParagraphSpec = checkBodyConversion
        "Converts multiple paragraphs to paragraphs"
        "bla di\nbla bla\n\nbla bla bla\n ding\n"
        (   "<p>bla di\nbla bla\n</p>"
         <> "<p>bla bla bla\n ding\n</p>")

convertHeaderSpec :: Int -> Spec
convertHeaderSpec w = checkBodyConversion ("Check header of weight " <> wString)
        markup body
            where wString = show w
                  markup = (take w $ repeat '*') <> "bla"
                  openTag = "<h" <> wString <> ">"
                  closeTag = "</h" <> wString <> ">"
                  body = openTag <> "bla" <> closeTag

escapeCharacterSpec :: String -> String -> Spec
escapeCharacterSpec input output = checkBodyConversion 
        ("Converts " <> input <> " to " <> output)
        input ("<p>" <> output <> "\n</p>")

convertCodeSpec :: Spec
convertCodeSpec = checkBodyConversion
        "It convert > at the start of the line to <pre>"
        ">def f():\n>    print('hi world')"
        ("<pre>def f():\n"
        <> "    print(&#39;hi world&#39;)\n"
        <> "</pre>")

orderedListSpec :: Spec
orderedListSpec = checkBodyConversion
        "It converts - at the start of the line to ordered list items"
        "-item one\n-item two\n-item three"
        (  "<ol><li><p>item one</p></li>"
        <> "<li><p>item two</p></li>"
        <> "<li><p>item three</p></li></ol>")


spec :: Spec
spec = do
        convertHeaderSpec 1
        convertHeaderSpec 2
        convertHeaderSpec 3
        newlineConvertSpec 0
        newlineConvertSpec 1
        newlineConvertSpec 2
        newlineConvertSpec 3
        newlineConvertSpec 4
        convertParagraphSpec
        escapeCharacterSpec "filler >" "filler &gt;"
        escapeCharacterSpec "<" "&lt;"
        escapeCharacterSpec "&" "&amp;"
        escapeCharacterSpec "\"" "&quot;"
        escapeCharacterSpec "'" "&#39;"
        convertCodeSpec
        convertMultipleParagraphSpec
        orderedListSpec

module HtmlGeneratorSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)
import Test.Hspec.QuickCheck (prop)

import HtmlGenerator (convert)

checkBodyConversion :: String -> String -> String -> Spec
checkBodyConversion message markup body = it message $ 
        (convert "Some title" markup) `shouldBe`
                ("<html><head><title>Some title</title></head><body>" <> body <> "</body></html>")

newlineConvertSpec :: Spec
newlineConvertSpec = prop
        "Converts non-negative number of newlines to an empty document" $
        \n -> convert "Some title" (take n $ repeat '\n')
                `shouldBe` "<html><head><title>Some title</title></head><body></body></html>"

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
convertHeaderSpec w = checkBodyConversion 
        ("Converts " <> (take w $ repeat '*') <> " to header of weight " <> wString)
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
        "Converts > at the start of the line to <pre>"
        ">def f():\n>    print('hi world')"
        ("<pre>def f():\n"
        <> "    print(&#39;hi world&#39;)\n"
        <> "</pre>")

orderedListSpec :: Spec
orderedListSpec = checkBodyConversion
        "Converts - at the start of the line to ordered list items"
        "-item one\n-item two\n-item three"
        (  "<ol><li><p>item one</p></li>"
        <> "<li><p>item two</p></li>"
        <> "<li><p>item three</p></li></ol>")


spec :: Spec
spec = describe "HtmlGenerator.convert" $ do
        convertHeaderSpec 1
        convertHeaderSpec 2
        convertHeaderSpec 3
        newlineConvertSpec
        convertParagraphSpec
        escapeCharacterSpec "filler >" "filler &gt;"
        escapeCharacterSpec "<" "&lt;"
        escapeCharacterSpec "&" "&amp;"
        escapeCharacterSpec "\"" "&quot;"
        escapeCharacterSpec "'" "&#39;"
        convertCodeSpec
        convertMultipleParagraphSpec
        orderedListSpec

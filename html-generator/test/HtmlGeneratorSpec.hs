module HtmlGeneratorSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec, Expectation)
import Test.Hspec.QuickCheck (prop)

import HtmlGenerator (convert)


bodyExpectation :: String -> String -> Expectation
bodyExpectation markup body = (convert "Some title" markup) `shouldBe`
                ("<html><head><title>Some title</title></head><body>" <> body <> "</body></html>")


checkBodyConversion :: String -> String -> String -> Spec
checkBodyConversion message markup body = it message $ bodyExpectation markup body


newlineConvertSpec :: Spec
newlineConvertSpec = prop
        "Converts non-negative number of newlines to an empty document" $
        \n -> bodyExpectation (take n $ repeat '\n') ""


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


convertHeaderSpec :: Spec
convertHeaderSpec = prop "Converts arbitrary number of * to header of correct weight" $
        \w -> bodyExpectation
                ((take w $ repeat '*') <> "My header")
                (if w > 0 then "<p><h" <> show w <> ">My header</h" <> show w <> "></p>" else "<p>My header\n</p>")


escapeCharacterSpec :: String -> String -> Spec
escapeCharacterSpec input output = checkBodyConversion 
        ("Converts " <> input <> " to " <> output)
        input ("<p>" <> output <> "\n</p>")


convertCodeSpec :: Spec
convertCodeSpec = checkBodyConversion
        "Converts > at the start of the line to <pre>"
        ">def f():\n>    print('hi world')"
        ("<p><pre>def f():\n"
        <> "    print(&#39;hi world&#39;)\n"
        <> "</pre></p>")


orderedListSpec :: Spec
orderedListSpec = checkBodyConversion
        "Converts - at the start of the line to ordered list items"
        "-item one\n-item two\n-item three"
        (  "<p><ol><li><p>item one</p></li>"
        <> "<li><p>item two</p></li>"
        <> "<li><p>item three</p></li></ol></p>")


spec :: Spec
spec = describe "HtmlGenerator.convert" $ do
        convertHeaderSpec
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

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
        "bla" "<p>bla</p>"


convertMultipleParagraphSpec :: Spec
convertMultipleParagraphSpec = checkBodyConversion
        "Converts multiple paragraphs to paragraphs"
        "bla di\nbla bla\n\nbla bla bla\n ding\n"
        (   "<p>bla di\nbla bla</p>"
         <> "<p>bla bla bla\n ding</p>")


convertHeaderSpec :: Spec
convertHeaderSpec = prop "Converts arbitrary number of * to header of correct weight" $
        \w -> bodyExpectation
                ((take w $ repeat '*') <> "My header")
                (if w > 0 then "<h" <> show w <> ">My header</h" <> show w <> ">" else "<p>My header</p>")


escapeCharacterSpec :: String -> String -> Spec
escapeCharacterSpec input output = checkBodyConversion 
        ("Converts " <> input <> " to " <> output)
        input ("<p>" <> output <> "</p>")


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

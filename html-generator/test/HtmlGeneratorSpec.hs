module HtmlGeneratorSpec (spec) where

import qualified Data.Text as T

import Test.Hspec (it, describe, shouldBe, Spec, Expectation)
import Test.Hspec.QuickCheck (prop)

import HtmlGenerator (convert)


showT :: (Show a) => a -> T.Text
showT = T.pack . show


bodyExpectation :: T.Text -> T.Text -> Expectation
bodyExpectation markup body = (convert "Some title" markup) `shouldBe`
                ("<html><head><title>Some title</title></head><body>" <> body <> "</body></html>")


checkBodyConversion :: String -> T.Text -> T.Text -> Spec
checkBodyConversion message markup body = it message $ bodyExpectation markup body


newlineConvertSpec :: Spec
newlineConvertSpec = prop
        "Converts non-negative number of newlines to an empty document" $
        \n -> bodyExpectation (T.pack . take n . repeat $ '\n') ""


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
                ((T.pack . take w . repeat $ '*') <> "My header")
                (if w > 0 then "<p><h" <> showT w <> ">My header</h" <> showT w <> "></p>" else "<p>My header\n</p>")


escapeCharacterSpec :: T.Text -> T.Text -> Spec
escapeCharacterSpec input output = checkBodyConversion 
        (T.unpack ("Converts " <> input <> " to " <> output))
        input ("<p>" <> output <> "\n</p>")


convertCodeSpec :: Spec
convertCodeSpec = checkBodyConversion
        "Converts > at the start of the line to <pre>"
        ">def f():\n>    print('hi world')"
        ("<p><pre><code>def f():\n"
        <> "    print(&#39;hi world&#39;)\n"
        <> "</code></pre></p>")


orderedListSpec :: Spec
orderedListSpec = checkBodyConversion
        "Converts - at the start of the line to ordered list items"
        "-item one\n-item two\n-item three"
        (  "<p><ol><li><p>item one</p></li>"
        <> "<li><p>item two</p></li>"
        <> "<li><p>item three</p></li></ol></p>")


convertInLineCode :: Spec
convertInLineCode = checkBodyConversion
        "Converts in line code to code"
        "bla di `code here` bla\n"
        "<p>bla di \n<code>code here</code> bla\n</p>"


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
        convertInLineCode

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


convertHeaderSpec :: Spec
convertHeaderSpec = prop "Converts arbitrary number of * to header of correct weight" $
        \w -> bodyExpectation
                ((T.pack . take w . repeat $ '*') <> "My header")
                (if w > 0 then "<p><h" <> showT w <> ">My header</h" <> showT w <> "></p>" else "<p>My header\n</p>")


escapeCharacterSpec :: T.Text -> T.Text -> Spec
escapeCharacterSpec input output = checkBodyConversion 
        (T.unpack ("Converts " <> input <> " to " <> output))
        input ("<p>" <> output <> "\n</p>")


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

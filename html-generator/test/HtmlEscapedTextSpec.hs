module HtmlEscapedTextSpec (spec) where

import qualified Data.Text as T

import Test.Hspec (it, describe, shouldBe, Spec, Expectation)

import HtmlGenerator.HtmlEscapedText (escape, printEscaped)


characterExpectation :: Char -> T.Text -> Expectation
characterExpectation c expected = (printEscaped . escape . T.singleton $ c) `shouldBe` expected


spec :: Spec
spec = describe "HtmlEscapedText" $ do
    it "Translates & to &amp;" $ characterExpectation '&' "&amp;"
    it "Translates < to &lt;" $ characterExpectation '<' "&lt;"
    it "Translates > to &gt;" $ characterExpectation '>' "&gt;"
    it "Translates \" to &quot;" $ characterExpectation '\"' "&quot;"
    it "Translates \' to &#39;" $ characterExpectation '\'' "&#39;"
    it "Translates e to e" $ characterExpectation 'e' "e"

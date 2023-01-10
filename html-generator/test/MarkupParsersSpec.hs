module MarkupParsersSpec (spec) where

import qualified Data.Text as T

import Test.Hspec (it, describe, shouldBe, Spec, Expectation)

import HtmlGenerator.MarkupParsers


documentExpectation :: T.Text -> [[MarkupToken]] -> Expectation
documentExpectation input expected = parseMarkup input
    `shouldBe` (Document . map Paragraph $ expected)


spec :: Spec
spec = describe "HtmlEscapedText" $ do
    it "Converts empty string to empty document" $ documentExpectation "" []
    it "Converts a header to document with single header" $ documentExpectation "* My header\n" [[Header 1 "My header"]]
    it "Converts normal text to document with normal text" $ documentExpectation "Some normal text\n\n" [[NormalText "Some normal text"]]
    it "Converts header and text" $ documentExpectation "** Hi mom\nSome normal text\n\n" [[Header 2 "Hi mom", NormalText "Some normal text"]]
    it "Converts normal text with asterisk in the middle" $ documentExpectation "Some normal* text\n\n" [[NormalText "Some normal* text"]]
    it "Converts normal text with angular bracket in the middle"
        $ documentExpectation
            "Some normal> text\n\n"
            [[NormalText "Some normal> text"]]
    it "Converts 2 paragraph document with normal text"
        $ documentExpectation
            "Some normal text\n\nSome more\n\n"
            [[NormalText "Some normal text"], [NormalText "Some more"]]
    it "Converts 2 multi-line paragraph documents with normal text"
        $ documentExpectation
            "* my header\nSome normal text\n\nSome more\n** My other header\n"
            [
                [Header 1 "my header", NormalText "Some normal text"]
                , [NormalText "Some more", Header 2 "My other header"]
            ]
    it "Converts 2 line code block"
        $ documentExpectation
            ">some code\n>some more\n\n"
            [[CodeBlock ["some code", "some more"]]]
    it "Converts 2 line code block followed by header"
        $ documentExpectation
            ">some code\n>some more\n* and this\n"
            [[CodeBlock ["some code", "some more"], Header 1 "and this"]]
    it "Converts normal text followed by 2 line code block followed by header"
        $ documentExpectation
            "Normal text\n>some code\n>some more\n* and this\n"
            [[NormalText "Normal text", CodeBlock ["some code", "some more"], Header 1 "and this"]]
    it "Converts normal text followed by 2 line ordered list"
        $ documentExpectation
            "Normal text\n-item 1\n-item 2\n"
            [[NormalText "Normal text", OrderedList ["item 1", "item 2"]]]
    it "Converts a header to document with single header with no leading whitespace"
        $ documentExpectation
            "*My header\n"
            [[Header 1 "My header"]]
    it "Converts multiline normal text"
        $ documentExpectation
            "Some text\nSome more\n\n"
            [[NormalText "Some text", NormalText "Some more"]]
    it "Converts more complicated multiline normal text"
        $ documentExpectation
            "bla di\nbla bla\n\nbla bla bla\n ding\n"
            [[NormalText "bla di", NormalText "bla bla"], [NormalText "bla bla bla", NormalText " ding"]]
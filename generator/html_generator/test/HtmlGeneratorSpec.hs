module Main where

import Test.Hspec
import HtmlGenerator

pendingTests :: Spec
pendingTests = do
    it "needs some tests" $ do
        pending

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
                  html = "<html><head><tile>Some title</title></head><body>"
                    <> openTag <> "bla" <> closeTag
                    <> "</body></html>"


main :: IO ()
main = hspec $ do
    emptyConvertSpec
    convertParagraphSpec
    convertHeaderSpec 1
    convertHeaderSpec 2
    convertHeaderSpec 3

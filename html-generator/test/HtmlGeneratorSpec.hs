module HtmlGeneratorSpec (spec) where

import qualified Data.Text as T

import Test.Hspec (describe, shouldBe, Spec, Expectation)
import Test.Hspec.QuickCheck (prop)

import HtmlGenerator (convert)


showT :: (Show a) => a -> T.Text
showT = T.pack . show


bodyExpectation :: T.Text -> T.Text -> Expectation
bodyExpectation markup body = (convert "Some title" markup) `shouldBe`
                ("<html><head><title>Some title</title></head><body>" <> body <> "</body></html>")


convertHeaderSpec :: Spec
convertHeaderSpec = prop "Converts arbitrary number of * to header of correct weight" $
        \w -> bodyExpectation
                ((T.pack . take w . repeat $ '*') <> "My header")
                (if w > 0 then "<p><h" <> showT w <> ">My header</h" <> showT w <> "></p>" else "<p>My header\n</p>")


spec :: Spec
spec = describe "HtmlGenerator.convert" $ do
        convertHeaderSpec

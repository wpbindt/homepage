module TitleSpec (spec) where

import Test.Hspec (it, describe, shouldBe, Spec)

import Site.Title


spec :: Spec
spec = do
    describe "Title.toFileName" $ do
        it "Converts titles with extension" $ 
                (toFileName ["hi"] "mom") `shouldBe` "hi.mom"
        it "Hyphenates multipart titles" $ 
                (toFileName ["hi", "there"] "mom") `shouldBe` "hi-there.mom"
        it "Does not add empty extensions" $ 
                (toFileName ["hi", "there"] "") `shouldBe` "hi-there"

    describe "Title.toHeader" $ do
        it "Converts single part title" $ 
                (toHeader ["hi"]) `shouldBe` "Hi"
        it "Converts multi part title" $ 
                (toHeader ["hi", "mom"]) `shouldBe` "Hi mom"

    describe "Title.toDirName" $ do
        it "Converts single part title" $
                (toDirName ["hi"]) `shouldBe` "hi"
        it "Hyphenates multipart titles" $ 
                (toDirName ["hi", "there"]) `shouldBe` "hi-there"

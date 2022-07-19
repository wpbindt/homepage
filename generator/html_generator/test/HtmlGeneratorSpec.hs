module Main where

import Test.Hspec

pendingTests :: Spec
pendingTests = do
    it "needs some tests" $ do
        pending

convertSpec :: Spec
convertSpec = describe "convert" $ do
    pendingTests


main :: IO ()
main = hspec $ do
    convertSpec

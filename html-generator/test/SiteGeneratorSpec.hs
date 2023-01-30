module SiteGeneratorSpec (spec) where

import qualified Data.Text as T
import Test.Hspec (it, describe, shouldBe, Spec, Expectation)

import SiteGenerator
import Directory.Directory


singlePageExpectation :: T.Text -> T.Text -> Expectation
singlePageExpectation markup expectedHtml = convertMarkupDirToHtmlDir inputDir `shouldBe` outputDir
        where inputDir = Directory "my-homepage" [notesInDir] []
              notesInDir = Directory "notes" [] [File "my-page.mu" markup]
              outputDir = Directory "static" [notesOutDir] [indexPage]
              notesOutDir = Directory "notes" [] [File "my-page.html" (htmlHead <> expectedHtml <> htmlTail)]
              htmlHead = "<html><head><title>My page</title></head><body>"
              htmlTail = "</body></html>"
              indexPage = File "index.html" indexContent
              indexContent = T.unlines [
                    "<html>"
                    , "<head>"
                    , "<title>My homepage</title>"
                    , "</head>"
                    , "<body>"
                    , "<h1>My homepage</h1>"
                    , "<h2>Notes</h2>"
                    , "<ul>"
                    , "<li><a href=\"notes/my-page.html\">My page</a></li>"
                    , "</ul>"
                    , "</body>"
                    , "</html>"
                ]

spec :: Spec
spec = describe "SiteGenerator.convertMarkupDirToHtmlDir" $ do
    it "converts an empty page to an empty page" $ singlePageExpectation "" ""
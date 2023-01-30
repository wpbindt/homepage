module SiteGeneratorSpec (spec) where

import qualified Data.Text as T
import Test.Hspec (it, describe, shouldBe, Spec, Expectation)

import SiteGenerator
import Directory.Directory


singlePageInputDir :: T.Text -> Directory
singlePageInputDir markup = Directory "my-homepage" [notesDir] []
        where notesDir = Directory "notes" [] [File "my-page.mu" markup]


singlePageOutputDir :: T.Text -> Directory
singlePageOutputDir expectedHtml = Directory "static" [notesDir] [indexPage]
        where notesDir = Directory "notes" [] [pageFile]
              pageFile = File "my-page.html" (htmlHead <> expectedHtml <> htmlTail)
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


singlePageExpectation :: T.Text -> T.Text -> Expectation
singlePageExpectation markup expectedHtml = convertMarkupDirToHtmlDir inputDir `shouldBe` outputDir
        where inputDir = singlePageInputDir markup
              outputDir = singlePageOutputDir expectedHtml


singlePageBodyExpectation :: T.Text -> T.Text -> Expectation
singlePageBodyExpectation markup expectedBody = actualBody `shouldBe` expectedBody
        where actualDir = convertMarkupDirToHtmlDir . singlePageInputDir $ markup
              actualNotesDir = head . getDirectories $ actualDir
              actualHtml = getContent . head . getFiles $ actualNotesDir
              header = T.length "<html><head><title>My page</title></head><body>"
              footer = T.length "</body></html>"
              actualBody = T.dropEnd footer . T.drop header $ actualHtml


spec :: Spec
spec = describe "SiteGenerator.convertMarkupDirToHtmlDir" $ do
    it "generates correct index and file tree" $ singlePageExpectation "" ""
    it "converts a header to a header" $ singlePageBodyExpectation "* My header\n" "<p><h1>My header</h1></p>"
    it "converts a 2-header to a 2-header" $ singlePageBodyExpectation "** My header\n" "<p><h2>My header</h2></p>"

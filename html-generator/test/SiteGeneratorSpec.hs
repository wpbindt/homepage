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


escapeCharacterSpec :: T.Text -> T.Text -> Spec
escapeCharacterSpec input output = it (T.unpack ("Converts " <> input <> " to " <> output)) $
        singlePageBodyExpectation
            (input <> "\n") 
            ("<p>" <> output <> "\n</p>")


spec :: Spec
spec = describe "SiteGenerator.convertMarkupDirToHtmlDir" $ do
    it "generates correct index and file tree" $ singlePageExpectation "" ""

    it "converts a header to a header" $ singlePageBodyExpectation "* My header\n" "<p><h1>My header</h1></p>"
    it "converts a 2-header to a 2-header" $ singlePageBodyExpectation "** My header\n" "<p><h2>My header</h2></p>"

    it "converts > at the start of the line to <pre><code>" $ singlePageBodyExpectation
        ">def f():\n>    print('hi world')\n"
        (  "<p><pre><code>def f():\n"
        <> "    print(&#39;hi world&#39;)\n"
        <> "</code></pre></p>")

    it "Converts - at the start of the line to ordered list items" $ singlePageBodyExpectation
        "-item one\n-item two\n-item three\n"
        (  "<p><ol><li><p>item one</p></li>"
        <> "<li><p>item two</p></li>"
        <> "<li><p>item three</p></li></ol></p>")

    it "Converts in line code to code" $ singlePageBodyExpectation
        "bla di `code here` bla\n"
        "<p>bla di \n<code>code here</code> bla\n</p>"

    it "Converts multiple paragraphs to paragraphs" $ singlePageBodyExpectation
        "bla di\nbla bla\n\nbla bla bla\n ding\n"
        (  "<p>bla di\nbla bla\n</p>"
        <> "<p>bla bla bla\n ding\n</p>")

    escapeCharacterSpec "e" "e"
    escapeCharacterSpec "filler >" "filler &gt;"
    escapeCharacterSpec "<" "&lt;"
    escapeCharacterSpec "&" "&amp;"
    escapeCharacterSpec "\"" "&quot;"
    escapeCharacterSpec "'" "&#39;"

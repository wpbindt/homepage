module RenderSiteSpec (spec) where

import qualified Data.Text as T
import Test.Hspec (it, describe, shouldBe, Spec)

import Site.RenderSite
import Site.Site
import Site.Title
import Directory.Directory


renderTSite :: Site T.Text -> Directory
renderTSite = renderSite id


indexPage :: Title -> T.Text
indexPage title = T.unlines [
        "<html>"
        , "<head>"
        , "<title>" <> plainTitle <> "</title>"
        , "</head>"
        , "<body>"
        , "<h1>" <> plainTitle <> "</h1>"
        , "</body>"
        , "</html>"
    ]
        where plainTitle = toHeader title


indexPageWithEmptySection :: Title -> Title -> T.Text
indexPageWithEmptySection title sectionTitle = T.unlines [
        "<html>"
        , "<head>"
        , "<title>" <> plainTitle <> "</title>"
        , "</head>"
        , "<body>"
        , "<h1>" <> plainTitle <> "</h1>"
        , "<h2>" <> toHeader sectionTitle <> "</h2>"
        , "<ul>"
        , "</ul>"
        , "</body>"
        , "</html>"
    ]
        where plainTitle = toHeader title

indexPageWithSomePages :: Title -> T.Text
indexPageWithSomePages title = T.unlines [
        "<html>"
        , "<head>"
        , "<title>" <> plainTitle <> "</title>"
        , "</head>"
        , "<body>"
        , "<h1>" <> plainTitle <> "</h1>"
        , "<h2>Some section</h2>"
        , "<ul>"
        , "<li><a href=\"some-section/page-1.html\">Page 1</a></li>"
        , "<li><a href=\"some-section/page-bla.html\">Page bla</a></li>"
        , "</ul>"
        , "</body>"
        , "</html>"
    ]
        where plainTitle = toHeader title


spec :: Spec
spec = do
    describe "Site.renderSite" $ do
        it "Renders index page with title" $ 
                (renderTSite (Site ["my", "homepage"] []))
                `shouldBe` (Directory "static" [] [File "index.html" (indexPage ["my", "homepage"])])
        it "Renders index page with title and section" $ 
                (renderTSite (Site ["my", "homepage"] [Section ["some", "section"] []]))
                `shouldBe` (Directory "static" 
                    [
                        Directory "some-section" [] []
                    ] 
                    [
                        File "index.html" (indexPageWithEmptySection ["my", "homepage"] ["some", "section"])
                    ]
                    )
        it "Renders index page with some pages" $ 
                (renderTSite (
                    Site 
                        ["my", "homepage"] 
                        [Section ["some", "section"] 
                            [
                                Page ["page", "1"] "Hi"
                                , Page ["page", "bla"] "There"
                            ]
                        ]
                ))
                `shouldBe` (Directory "static" 
                    [
                        Directory "some-section" []
                            [
                                File "page-1.html" "Hi"
                                , File "page-bla.html" "There"
                            ]
                    ] 
                    [
                        File "index.html" (indexPageWithSomePages ["my", "homepage"])
                    ])

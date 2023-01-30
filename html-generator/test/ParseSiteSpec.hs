module ParseSiteSpec (spec) where

import qualified Data.Text as T
import Test.Hspec (it, describe, shouldBe, Spec, Expectation)

import Site.ParseSite
import Site.Site
import Site.Title
import Directory.Directory


parseTSite :: Directory -> Site T.Text
parseTSite = parseSite id


sectionTestCase :: [Page T.Text] -> Expectation
sectionTestCase pages = (parseTSite (Directory "content" [Directory "notes" [] (map convertPage' pages)] []))
                        `shouldBe` (Site ["content"] [Section ["notes"] pages])
                            where convertPage' (Page title content) = File (toFileName title "mu") content


spec :: Spec
spec = do
    describe "Site.parseSite" $ do
        it "Converts empty subdirectory to empty section" $ 
                (parseTSite (Directory "content" [Directory "notes" [] []] [])) 
                `shouldBe` Site ["content"] [Section ["notes"] []]
        it "Converts empty subdirectory with multipart title to empty section" $
                (parseTSite (Directory "my-homepage" [Directory "notes" [] []] []))
                `shouldBe` Site ["my", "homepage"] [Section ["notes"] []]
        it "Converts empty subdirectories to empty sections" $ 
                (parseTSite (Directory "content" [Directory "notes" [] [], Directory "other" [] []] [])) 
                `shouldBe` Site ["content"] [Section ["notes"] [], Section ["other"] []]
        it "Converts single page section" $
            sectionTestCase [Page ["mypage"] ""]
        it "Converts multi page section" $
            sectionTestCase [Page ["mypage"] "", Page ["another"] "with content"]
        it "Dehyphenates section names" $
                (parseTSite (Directory "content" [Directory "notes-boy" [] []] [])) 
                `shouldBe` Site ["content"] [Section ["notes", "boy"] []]
        it "Dehyphenates page names" $
            sectionTestCase [Page ["my", "page"] ""]

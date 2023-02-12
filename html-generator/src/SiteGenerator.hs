module SiteGenerator where

import System.Environment

import HtmlGenerator.Convert
import Directory.Directory
import Directory.Tree
import Directory.Write
import qualified HtmlGenerator.Html as Html
import qualified Markup.Markup as Markup
import Markup.MarkupParsers
import Site.ParseSite
import Site.RenderSite
import Site.Site
import Site.Title


convertMarkupDirToHtmlDir :: Directory -> Directory
convertMarkupDirToHtmlDir = renderHtmlSite . convertMarkupSite . parseMarkupSite


parseMarkupSite :: Directory -> Site Markup.Document
parseMarkupSite = parseSite parseMarkup . filterByExtension "mu"


convertMarkupPage :: Page Markup.Document -> Page Html.Html
convertMarkupPage (Page title content) = Page title (convertMarkupToHtml (toHeader title) content)


convertMarkupSite :: Site Markup.Document -> Site Html.Html
convertMarkupSite = convertSite convertMarkupPage


renderHtmlSite :: Site Html.Html -> Directory
renderHtmlSite = renderSite Html.render


mainAlgo :: FilePath -> IO ()
mainAlgo filepath = (convertMarkupDirToHtmlDir <$> tree filepath) >>= write


main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [contentDir] -> mainAlgo contentDir
        _ -> putStrLn "Wrong usage"

module Site.RenderSite where

import qualified Data.Text as T

import Directory.Directory
import Site.Site
import Site.Title


type ContentRenderer a = a -> T.Text


renderSite :: ContentRenderer a -> Site a -> Directory
renderSite renderer site@(Site _ sections) = Directory 
                    "static"
                    (map (renderSection renderer) sections)
                    [renderIndexPage site]


renderIndexPage :: Site a -> File
renderIndexPage site = File "index.html" (renderIndexContent site)


renderIndexContent :: Site a -> T.Text
renderIndexContent (Site title sections) = T.unlines $
            header 
            ++ ["<body>" , "<h1>" <> plainTitle <> "</h1>"]
            ++ renderedSections
            ++ ["</body>" , "</html>"]
        where plainTitle = toHeader title
              header = [
                      "<html>" 
                      , "<head>" 
                      , "<title>" <> plainTitle <> "</title>" 
                      , "<link rel=\"stylesheet\" href=\"styles.css\">"
                      , "</head>" 
                  ]
              renderedSections = concatMap renderIndexSection sections


renderIndexSection :: Section a -> [T.Text]
renderIndexSection (Section title pages) = [renderedTitle, "<ul>"] ++ renderedPages ++ ["</ul>"]
        where renderedTitle = "<h2>" <> toHeader title <> "</h2>"
              renderedPages = map (pageToAnchor title) pages


pageToAnchor :: Title -> Page a -> T.Text
pageToAnchor sectionTitle (Page pageTitle _) = "<li><a href=\"" 
                                               <> sectionDir <> "/"
                                               <> pageFile
                                               <> "\">" <> pageHeader
                                               <> "</a></li>"
                                            where sectionDir = T.pack . toDirName $ sectionTitle
                                                  pageFile = T.pack $ toFileName pageTitle "html"
                                                  pageHeader = toHeader $ pageTitle


renderSection :: ContentRenderer a -> Section a -> Directory
renderSection renderer (Section title pages) = Directory (toDirName title) [] (map (renderPage renderer) pages)


renderPage :: ContentRenderer a -> Page a -> File
renderPage renderer (Page title content) = File (toFileName title "html") (renderer content)

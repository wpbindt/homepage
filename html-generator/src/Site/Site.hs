module Site.Site where


import Site.Title


data Page a = Page Title a deriving (Eq, Show)
data Section a = Section Title [Page a] deriving (Eq, Show)
data Site a = Site Title [Section a] deriving (Eq, Show)


convertSite :: (Page a -> Page b) -> Site a -> Site b
convertSite converter (Site title sections) = Site title (map (convertSection converter) sections)


convertSection :: (Page a -> Page b) -> Section a -> Section b
convertSection converter (Section title pages) = Section title (map converter pages)

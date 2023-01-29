module Site.Site where


import Site.Title


data Page a = Page Title a deriving (Eq, Show)
data Section a = Section Title [Page a] deriving (Eq, Show)
data Site a = Site Title [Section a] deriving (Eq, Show)

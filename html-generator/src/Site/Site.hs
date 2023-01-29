module Site.Site where


import Site.Title


data Page a = Page Title a deriving (Functor, Eq, Show)
data Section a = Section Title [Page a] deriving (Functor, Eq, Show)
data Site a = Site Title [Section a] deriving (Functor, Eq, Show)

module Site.Site where


import Site.Title


data Page a = Page Title a deriving (Functor, Eq)
data Section a = Section Title [Page a] deriving (Functor, Eq)
data Site a = Site Title [Section a] deriving (Functor, Eq)

module HtmlGenerator.Html
    ( Html
    , Title
    , Structure
    , html_
    , code_
    , ul_
    , ol_
    , p_
    , h1_
    , h_
    , append_
    , render
    ) where

newtype Html = Html String deriving (Eq, Show)

newtype Structure = Structure String deriving (Show, Eq)


instance Semigroup Structure where
    (<>) = append_


instance Monoid Structure where
    mempty = Structure ""


type Title = String


el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


html_ :: Title -> Structure -> Html
html_ title (Structure body) = Html 
    (el "html"
        ((el "head" . el "title" $ (escape title))
        <> (el "body" body)
        )
    )


makeListItem :: Structure -> String
makeListItem (Structure s) = el "li" s


ul_ :: [Structure] -> Structure
ul_ structures = Structure 
    ( el "ul" 
        ( concatMap makeListItem structures
        )
    )


ol_ :: [Structure] -> Structure
ol_ structures = Structure 
    ( el "ol" 
        ( concatMap makeListItem structures
        )
    )

code_ :: String -> Structure
code_ = Structure . el "pre" . escape


p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h_ :: Int -> String -> Structure
h_ w c = Structure . el ('h':show w) . escape $ c


append_ :: Structure -> Structure -> Structure
append_ (Structure s1) (Structure s2) = Structure $ s1 <> s2

render :: Html -> String
render (Html s) = s

escapeChar :: Char -> String
escapeChar c = 
    case c of
        '<' -> "$lt;"
        '>' -> "$gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]

escape :: String -> String
escape = concat . map escapeChar

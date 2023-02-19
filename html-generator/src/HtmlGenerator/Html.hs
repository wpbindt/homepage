module HtmlGenerator.Html
    ( Html
    , Title
    , html_
    , code_
    , pre_
    , ul_
    , ol_
    , p_
    , plain_
    , h_
    , render
    , makeBasicHtml
    ) where

import qualified Data.Text as T

import HtmlGenerator.HtmlEscapedText (HTMLEscapedText, escape, printEscaped)


type Title = T.Text
type Attribute = (T.Text, T.Text)
data Tag = Tag T.Text [Attribute] deriving (Eq, Show)
data Html = Html Tag [Html] | PlainText HTMLEscapedText deriving (Eq, Show)


render :: Html -> T.Text
render (PlainText escapedText) = printEscaped escapedText
render (Html tag htmls) = (openTag tag) <> (T.concat . map render $ htmls) <> (closeTag tag)


openTag :: Tag -> T.Text
openTag (Tag tag attributes) = "<" <> tag <> renderAttributes attributes <> ">"


renderAttributes :: [Attribute] -> T.Text
renderAttributes [] = ""
renderAttributes attributes = " " <> T.intercalate " " (map renderAttribute attributes)

renderAttribute :: Attribute -> T.Text
renderAttribute (key, value) = key <> "=\"" <> value <> "\""


closeTag :: Tag -> T.Text
closeTag (Tag tag _) = "</" <> tag <> ">"


plain_ :: T.Text -> Html
plain_ = PlainText . escape


el :: T.Text -> [Html] -> Html
el tag = Html (Tag tag []) 


simpleTag :: T.Text -> T.Text -> Html
simpleTag tag = el tag . pure . PlainText . escape


h_ :: Int -> T.Text -> Html
h_ weight = simpleTag . T.pack $ "h" <> (show weight)


code_ :: T.Text -> Html
code_ = simpleTag "code"


pre_ :: [Html] -> Html
pre_ = el "pre"


title_ :: Title -> Html
title_ = simpleTag "title"


html_ :: [Html] -> Html
html_ = el "html"


p_ :: [Html] -> Html
p_ = el "p"


body_ :: [Html] -> Html
body_ = el "body"


head_ :: [Html] -> Html
head_ = el "head"


makeListItem :: Html -> Html
makeListItem = Html (Tag "li" []) . pure


ul_ :: [Html] -> Html
ul_ = el "ul" . map makeListItem


ol_ :: [Html] -> Html
ol_ = el "ol" . map makeListItem


stylesheet :: Html
stylesheet = Html (Tag "link" [("rel", "stylesheet"), ("href", "../styles.css")]) []


makeBasicHtml :: Title -> [Html] -> Html
makeBasicHtml title tags = html_ $
        [ head_ [title_ title, stylesheet]
        , body_ tags
        ]

module HtmlGenerator.Convert where

import qualified Data.Text as T

import qualified HtmlGenerator.Markup as Markup
import qualified HtmlGenerator.Html as Html

convertStructure :: Markup.Structure -> Html.Html
convertStructure structure = case structure of
    Markup.Heading w c -> Html.h_ w (T.pack c)
    Markup.Paragraph c -> Html.p_ . pure . Html.plain_ . T.pack $ c
    Markup.OrderedList list -> Html.ol_ . map (Html.p_ . pure . Html.plain_ . T.pack) $ list
    Markup.CodeBlock c -> Html.code_ (T.pack c)


convertDocument :: Markup.Document -> [Html.Html]
convertDocument = map convertStructure

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.makeBasicHtml title . convertDocument

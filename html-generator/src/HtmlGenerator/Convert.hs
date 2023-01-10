module HtmlGenerator.Convert where

import qualified Data.Text as T

import qualified HtmlGenerator.Markup as Markup
import qualified HtmlGenerator.MarkupParsers as NewMarkup
import qualified HtmlGenerator.Html as Html



convertMarkupDocument :: NewMarkup.Document -> [Html.Html]
convertMarkupDocument (NewMarkup.Document paragraphs) = map convertParagraph paragraphs


convertParagraph :: NewMarkup.Paragraph -> Html.Html
convertParagraph (NewMarkup.Paragraph tokens) = Html.p_ $ map convertToken tokens


convertToken :: NewMarkup.MarkupToken -> Html.Html
convertToken token = case token of
    (NewMarkup.Header w c) -> Html.h_ w c
    (NewMarkup.OrderedList cs) -> Html.ol_ . map (Html.p_ . pure . Html.plain_) $ cs
    (NewMarkup.CodeBlock cs) -> Html.code_ . T.unlines $ cs
    (NewMarkup.NormalText c) -> Html.plain_ (c `T.snoc` '\n')


convertMarkupToHtml :: Html.Title -> NewMarkup.Document -> Html.Html
convertMarkupToHtml title = Html.makeBasicHtml title . convertMarkupDocument


convert :: Html.Title -> NewMarkup.Document -> Html.Html
convert title = Html.makeBasicHtml title . convertMarkupDocument

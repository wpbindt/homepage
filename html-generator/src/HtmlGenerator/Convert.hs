module HtmlGenerator.Convert where

import qualified Data.Text as T

import qualified HtmlGenerator.MarkupParsers as Markup
import qualified HtmlGenerator.Html as Html


convertMarkupDocument :: Markup.Document -> [Html.Html]
convertMarkupDocument (Markup.Document paragraphs) = map convertParagraph paragraphs


convertParagraph :: Markup.Paragraph -> Html.Html
convertParagraph (Markup.Paragraph tokens) = Html.p_ $ map convertToken tokens


convertToken :: Markup.MarkupToken -> Html.Html
convertToken token = case token of
    (Markup.Header w c) -> Html.h_ w c
    (Markup.OrderedList cs) -> Html.ol_ . map (Html.p_ . pure . Html.plain_) $ cs
    (Markup.CodeBlock cs) -> Html.code_ . T.unlines $ cs
    (Markup.NormalText c) -> Html.plain_ (c `T.snoc` '\n')


convertMarkupToHtml :: Html.Title -> Markup.Document -> Html.Html
convertMarkupToHtml title = Html.makeBasicHtml title . convertMarkupDocument


convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.makeBasicHtml title . convertMarkupDocument

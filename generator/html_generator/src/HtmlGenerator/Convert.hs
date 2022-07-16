module HtmlGenerator.Convert where

import qualified HtmlGenerator.Markup as Markup
import qualified HtmlGenerator.Html as Html

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure = case structure of
    Markup.Heading w c -> Html.h_ w c
    Markup.Paragraph c -> Html.p_ c
    Markup.OrderedList list -> Html.ol_ . map Html.p_ $ list
    Markup.CodeBlock c -> Html.code_ c


convertDocument :: Markup.Document -> Html.Structure
convertDocument = foldMap convertStructure

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . convertDocument


main :: IO ()
main = putStrLn . show . convert "buh" . Markup.parse $ "*Main title\nfirst\nparagraph\n\n next paragraph\n   \nthird\n**Hi\n>Code\n>Line"

module HtmlGenerator (convert) where
    
import qualified Data.Text as T

import qualified HtmlGenerator.Convert as Convert
import qualified HtmlGenerator.Html as Html
import qualified Markup.Markup as Markup
import qualified Markup.MarkupParsers as Parser


convert :: Html.Title -> T.Text -> T.Text
convert title input = Html.render
                      . Convert.convertMarkupToHtml title
                      . Parser.parseMarkup
                      $ input `T.snoc` '\n'

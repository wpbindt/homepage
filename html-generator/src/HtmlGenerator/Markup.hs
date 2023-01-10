module HtmlGenerator.Markup where

import qualified Data.Text as T
import qualified HtmlGenerator.MarkupParsers as New


type Document = [Structure]


data Structure = 
    Paragraph String
    | Heading Int String
    | CodeBlock String
    | OrderedList [String]
    deriving Show


-- Main parsing algo
parse :: String -> Document
parse input = acl . New.parseMarkup . T.pack $ input ++ "\n"


acl :: New.Document -> Document
acl (New.Document paragraphs) = concatMap aclParagraph paragraphs


aclParagraph :: New.Paragraph -> [Structure]
aclParagraph (New.Paragraph tokens) = map aclToken $ collapseNormalText tokens


collapseNormalText :: [New.MarkupToken] -> [New.MarkupToken]
collapseNormalText ts = foldr accu [] ts
    where accu (New.NormalText x) ((New.NormalText y):rest) = (New.NormalText $ x `T.append` T.cons '\n' y):rest
          accu t tokens = t:tokens


aclToken :: New.MarkupToken -> Structure
aclToken (New.NormalText x) = Paragraph . T.unpack $ x
aclToken (New.Header w x) = Heading w (T.unpack x)
aclToken (New.CodeBlock codeLines) = CodeBlock . unlines . map T.unpack $ codeLines
aclToken (New.OrderedList listItems) = OrderedList . map T.unpack $ listItems

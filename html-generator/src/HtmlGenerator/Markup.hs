module HtmlGenerator.Markup where

import qualified Data.Text as T
import qualified HtmlGenerator.MarkupParsers as New


type Document = [Structure]

type Line = String

data Structure = 
    Paragraph String
    | Heading Int String
    | CodeBlock String
    | OrderedList [String]
    deriving Show

data ParsingContext = 
    ParagraphCX [String]
    | HeadingCX Int String
    | CodeBlockCX [String]
    | OrderedListCX [String]
    | Empty


renderContext :: ParsingContext -> Maybe Structure
renderContext (ParagraphCX ls) = Just . Paragraph . unlines $ ls
renderContext (HeadingCX w c) = Just (Heading w c)
renderContext (CodeBlockCX ls) = Just . CodeBlock . unlines $ ls
renderContext (OrderedListCX list) = Just . OrderedList $ list
renderContext Empty = Nothing


getNewContext :: ParsingContext -> ParsingContext -> ParsingContext
getNewContext (ParagraphCX l) (ParagraphCX l') = ParagraphCX (l ++ l')
getNewContext (CodeBlockCX l) (CodeBlockCX l') = CodeBlockCX (l ++ l')
getNewContext (OrderedListCX l) (OrderedListCX l') = OrderedListCX (l ++ l')
getNewContext _ context = context


getNewStructure :: ParsingContext -> ParsingContext -> Maybe Structure
getNewStructure (ParagraphCX _) (ParagraphCX _) = Nothing
getNewStructure (CodeBlockCX _) (CodeBlockCX _) = Nothing
getNewStructure (OrderedListCX _) (OrderedListCX _) = Nothing
getNewStructure context _ = renderContext context


parseHeading :: String -> ParsingContext
parseHeading s = HeadingCX weight content
    where weight = length . takeWhile ((==) '*') $ s
          content = dropWhile ((==) '*') s


parseLine :: Line -> ParsingContext
parseLine line = case line of
    '*':_ -> parseHeading line
    '>':_ -> CodeBlockCX [tail line]
    '-':_ -> OrderedListCX [tail line]
    _ -> ParagraphCX [line]



-- Main parsing algo
parse :: String -> Document
parse = acl . New.parseMarkup . T.pack


parseLines :: ParsingContext -> [String] -> Document
parseLines context remainingLines =
    case remainingLines of
        [] -> maybePrepend (renderContext context) []
        currentLine : rest ->
            if (trim currentLine) == ""
                then maybePrepend (renderContext context) (parseLines Empty rest)
                else
                    let 
                        parsedLine = parseLine currentLine
                        newContext = getNewContext context parsedLine
                        newStructure = getNewStructure context parsedLine
                    in maybePrepend newStructure (parseLines newContext rest)



-- Generic helper functions
maybePrepend :: Maybe a -> [a] -> [a]
maybePrepend (Just s) l = s : l
maybePrepend Nothing l = l


trim :: String -> String
trim = unwords . words


acl :: New.Document -> Document
acl (New.Document paragraphs) = concatMap aclParagraph paragraphs


aclParagraph :: New.Paragraph -> [Structure]
aclParagraph (New.Paragraph tokens) = map aclToken tokens


aclToken :: New.MarkupToken -> Structure
aclToken (New.NormalText x) = Paragraph . T.unpack $ x
aclToken (New.Header w x) = Heading w (T.unpack x)
aclToken (New.CodeBlock codeLines) = CodeBlock . unlines . map T.unpack $ codeLines
aclToken (New.OrderedList listItems) = OrderedList . map T.unpack $ listItems

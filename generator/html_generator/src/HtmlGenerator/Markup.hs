module HtmlGenerator.Markup where

import Data.Maybe


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
getNewStructure (ParagraphCX s1) (ParagraphCX s2) = Nothing
getNewStructure (CodeBlockCX s1) (CodeBlockCX s2) = Nothing
getNewStructure (OrderedListCX s1) (OrderedListCX s2) = Nothing
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
parse = parseLines Empty . lines


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


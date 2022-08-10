import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative


data MarkupDocument = Markup [LowerLevelMarkup] deriving Show
data LowerLevelMarkup = Header Int T.Text 
                        | PlainText T.Text 
                        | Bold T.Text
                        | Code [T.Text]
                        | List [T.Text]
                            deriving Show


block :: Parser T.Text
block = T.pack <$> manyTill anyChar (string "\n\n")


blocks :: Parser [T.Text]
blocks = many block


countChar :: Char -> Parser Int
countChar c = length <$> many1 (char c)


header :: Parser LowerLevelMarkup
header = Header <$> (countChar '*') <*> (takeTill isEndOfLine)


bold :: Parser LowerLevelMarkup
bold = Bold . T.pack <$> do
    char '`'
    manyTill anyChar (char '`')


codeLine :: Parser T.Text
codeLine = (string "\n>" <|> string "\rn")
           *> takeTill isEndOfLine


codeBlock :: Parser LowerLevelMarkup
codeBlock = Code <$> (many1 codeLine)


listItem :: Parser T.Text
listItem = do
    string "\n-" <|> string "\r-"
    takeTill isEndOfLine


orderedList :: Parser LowerLevelMarkup
orderedList = List <$> (many1 listItem)


module HtmlGenerator (main, convert) where
    
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import qualified HtmlGenerator.Convert as Convert
import qualified HtmlGenerator.Html as Html
import qualified HtmlGenerator.Markup as Markup
import qualified HtmlGenerator.MarkupParsers as Parser


convert :: Html.Title -> T.Text -> T.Text
convert title input = Html.render
                      . Convert.convertMarkupToHtml title
                      . Parser.parseMarkup
                      $ input `T.snoc` '\n'


askPermission :: String -> IO Bool
askPermission prompt = do
    putStrLn prompt
    putStrLn "Are you sure? [y/N]"
    answer <- getLine
    return (answer == "y")


safeWrite :: FilePath -> T.Text -> IO ()
safeWrite path contents = do
    fileExists <- doesFileExist path
    doWrite <- if fileExists then askPermission "File exists" else return True
    if doWrite then TIO.writeFile path contents else putStrLn "quitting"


readConvertWrite :: Html.Title -> IO T.Text -> (T.Text -> IO ()) -> IO ()
readConvertWrite title read' write = convert title <$> read' >>= write


takeBaseNameT :: String -> T.Text
takeBaseNameT = T.pack . takeBaseName


main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [inputFile, outputFile] ->
            readConvertWrite
                (takeBaseNameT inputFile)
                (TIO.readFile inputFile)
                (safeWrite outputFile)
        _ -> 
            putStrLn "Wrong usage"

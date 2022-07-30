module HtmlGenerator (main, convert) where
    
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import qualified HtmlGenerator.Convert as Convert
import qualified HtmlGenerator.Html as Html
import qualified HtmlGenerator.Markup as Markup


convert :: Html.Title -> String -> String
convert title = T.unpack . Html.render . Convert.convert title . Markup.parse


askPermission :: String -> IO Bool
askPermission prompt = do
    putStrLn prompt
    putStrLn "Are you sure? [y/N]"
    answer <- getLine
    return (answer == "y")


safeWrite :: FilePath -> String -> IO ()
safeWrite path contents = do
    fileExists <- doesFileExist path
    doWrite <- if fileExists then askPermission "File exists" else return True
    if doWrite then writeFile path contents else putStrLn "quitting"


readConvertWrite :: Html.Title -> IO String -> (String -> IO ()) -> IO ()
readConvertWrite title read' write = convert title <$> read' >>= write


main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [] -> 
            readConvertWrite "dummy" getContents putStr
        [inputFile, outputFile] -> 
            readConvertWrite (T.pack . takeBaseName $ inputFile) (readFile inputFile) (safeWrite outputFile)
        _ -> 
            putStrLn "Wrong usage"

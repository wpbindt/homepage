module Directory.Write where

import Data.Text.IO as TIO
import System.Directory

import Directory.Directory


write :: Directory -> IO ()
write directory@(Directory path _ _) = do
    alreadyExists <- doesDirectoryExist path
    if alreadyExists then Prelude.putStrLn ("Directory \"" ++ path ++ "\" already exists, skipping")
    else writeDirectory directory


writeDirectory :: Directory -> IO ()
writeDirectory (Directory path directories files) = do
    createDirectory path
    _ <- mapM writeToFile files
    _ <- mapM writeDirectory directories
    return ()


writeToFile :: File -> IO ()
writeToFile (File path content) = TIO.writeFile path content

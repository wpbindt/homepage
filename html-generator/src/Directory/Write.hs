module Directory.Write where

import Data.Text.IO as TIO
import System.Directory

import Directory.Directory


write :: Directory -> IO ()
write (Directory path directories files) = do
    alreadyExists <- doesDirectoryExist path
    if alreadyExists then Prelude.putStrLn ("Directory \"" ++ path ++ "\" already exists, skipping")
    else do
        createDirectoryIfMissing True path
        _ <- mapM writeToFile files
        _ <- mapM write directories
        return ()


writeToFile :: File -> IO ()
writeToFile (File path content) = TIO.writeFile path content

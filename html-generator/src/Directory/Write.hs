module Directory.Write where

import Data.Text.IO as TIO
import System.Directory
import System.FilePath

import Directory.Directory


write :: Directory -> IO ()
write directory@(Directory path _ _) = do
    alreadyExists <- doesDirectoryExist path
    if alreadyExists then Prelude.putStrLn ("Directory \"" ++ path ++ "\" already exists, skipping")
    else writeDirectory "." directory


writeDirectory :: FilePath -> Directory -> IO ()
writeDirectory parentPath (Directory path directories files) = do
    createDirectory absoluteBasePath
    _ <- mapM (writeToFile absoluteBasePath) files
    _ <- mapM (writeDirectory absoluteBasePath) directories
    return ()
    where absoluteBasePath = parentPath </> path


writeToFile :: FilePath -> File -> IO ()
writeToFile parentPath (File path content) = TIO.writeFile (parentPath </> path) content

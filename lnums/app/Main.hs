module Main (main) where

import Data.Char

main :: IO ()
main = do
    line <- getLine
    putStrLn (map toUpper line)

interactiveLines :: Int -> IO ()
interactiveLines counter = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn (show counter ++ ". " ++ line)
            interactiveLines (counter + 1)

parseArguments :: [String] -> FilePath
parseArguments [filePath] = filePath
parseArguments _ = undefined


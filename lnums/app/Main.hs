module Main (main) where

import Data.Maybe
import System.Environment

readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents)

type NumberedLines = [(Int, String)]

main :: IO ()
main = putStrLn "someFunc"
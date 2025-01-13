module Main (main) where

import Lib
import Data.Maybe
import System.Environment

readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents)

main :: IO ()
main = putStrLn "Hello, Haskell!"
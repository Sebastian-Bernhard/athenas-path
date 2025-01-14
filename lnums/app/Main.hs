module Main (main) where

import Lib
import Data.Maybe
import System.Environment

readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents)

printHelpText :: String -> IO ()
printHelpText msg = do
    putStrLn (msg ++ "\n")
    progName <- getProgName
    putStrLn ("Usage: " ++ progName ++ " <filename>")

parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing

main :: IO ()
main = do
    cliArgs <- getArgs
    let mFilePath = parseArguments cliArgs
    maybe
        (printHelpText "Missing filename")
        ( \filePath -> do
            fileLines <- readLines filePath
            let numbered = numberAndIncrementNonEmptyLines fileLines
                prettyNumbered = prettyNumberedLines
                    PadLeft numbered
            mapM_ putStrLn prettyNumbered
        )
        mFilePath
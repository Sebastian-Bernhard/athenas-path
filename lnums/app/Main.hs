module Main (main) where

import Lib
import System.Environment
import Data.Maybe

data LineNumberOption
    = ReverseNumbering
    | SkipEmptyLines
    | LeftAlign
    deriving (Eq)

lnOptionFromString :: String -> Maybe LineNumberOption
lnOptionFromString "--reverse" = Just ReverseNumbering
lnOptionFromString "--skip-empty" = Just SkipEmptyLines
lnOptionFromString "--left-align" = Just LeftAlign
lnOptionFromString _ = Nothing

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <options> <filename>")
  putStrLn "\n"
  putStrLn " Options:"
  putStrLn "   --reverse      - Reverse the numbering"
  putStrLn "   --skip-empty   - Skip numbering empty lines"
  putStrLn "   --left-align   - Use left-aligned line numbers"

{-
printHelpText :: String -> IO ()
printHelpText msg = do
    putStrLn (msg ++ "\n")
    progName <- getProgName
    putStrLn ("Usage: " ++ progName ++ " <filename>")
-}
{-
parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing
-}

-- assume a fixed order of arguments, maybe options, filename last
parseArguments :: [String] -> (Maybe FilePath, [LineNumberOption])
parseArguments args = case reverse args of
    [] -> (Nothing, [])
    (filename : options) ->
        ( Just filename,
            mapMaybe lnOptionFromString options -- optional arguments
        )

readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents) -- split file into single lines

main :: IO ()
main = do
    cliArgs <- getArgs
    
    -- given options form the cliArgs guide the control flow of the program
    
    let (mFilePath, options) = parseArguments cliArgs

        numberFunction =
            if SkipEmptyLines `elem` options
                then numberNonEmptyLines
                else numberAllLines

        padMode =
            if LeftAlign `elem` options
                then PadRight
                else PadLeft

        go filePath = do
            fileLines <- readLines filePath
            let numbered = numberFunction fileLines
                prettyNumbered = prettyNumberedLines padMode numbered
                revNumbered = numberFunction (reverse fileLines)
                revPretty = reverse (prettyNumberedLines padMode revNumbered)
            mapM_
                putStrLn
                    ( if ReverseNumbering `elem` options
                        then revPretty
                        else prettyNumbered
                    )
    maybe
        (printHelpText "Missing filename")
        go
        mFilePath

{-
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
-}
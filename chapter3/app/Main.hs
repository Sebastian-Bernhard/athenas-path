module Main (main) where

import Data.Char
import System.Environment

interactiveLines :: Int -> IO ()
interactiveLines counter = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn (show counter ++ ". " ++ line)
            interactiveLines (counter + 1)

-- extract the value from the Maybe type
-- a beeing the default value, if a is Nothing
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just v) = v
fromMaybe v Nothing = v

-- reading and printing a command line argument

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
        (\filePath -> putStrLn filePath)
        mFilePath

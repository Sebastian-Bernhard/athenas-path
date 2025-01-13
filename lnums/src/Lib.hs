module Lib 
    (
        NumberedLine,
        NumberedLines,
        numberAllLines,
        isEmpty
    ) where
import Data.Char

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

numberAllLines :: [String] -> NumberedLines
numberAllLines lines =
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        -- create mapping with current line
        -- recursively build the test of the numbered lines
        go counter (line:rest) = (Just counter, line) : go (counter + 1) rest
    -- call go func, start the recursion by counter = 1 and the list of lines
    in go 1 lines

-- impl the func `lines` that splits a string into lines by '\n'
{-
go is a helper function that processes the input string.
go "" "" = [] handles the case where both the current line and the remaining text are empty.
go line "" = [line] handles the case where the remaining text is empty but there is a current line to be added to the result.
go line (x : xs) processes each character x in the remaining text xs.
If x is a newline character ('\n'), it adds the current line to the result and starts a new line.
Otherwise, it appends the character x to the current line and continues processing the remaining text.
-}
lines' :: String -> [String]
lines' "" = []
lines' text =
  let go "" "" = []
      go line "" = [line]
      go line (x : xs)
        | x == '\n' = line : go "" xs
        | otherwise = go (line ++ [x]) xs
    in go "" text

isEmpty :: String -> Bool
isEmpty str = 
    null str -- check if the string is empty
    || all (\s -> not (isPrint s) || isSeparator s) str

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber text =
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        go counter (x : xs) = 
            let mNumbering = if shouldNumber x then Just counter else Nothing
                newCounter = if shouldIncr x then counter + 1 else counter
            in (mNumbering, x) : go newCounter xs
   in go 1 text


    
module Lib
  ( NumberedLine,
    NumberedLines,
    isEmpty,
    isNotEmpty,
    numberLines,
    numberAllLines,
    numberNonEmptyLines,
    numberAndIncrementNonEmptyLines,
    prettyNumberedLines,
    PadMode (..),
    pad,
    padLeft,
    padRight,
  )
where
import Data.Char

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

-- number all lines without case handeling any characters
{-
numberAllLines :: [String] -> NumberedLines
numberAllLines lines =
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        -- create mapping with current line
        -- recursively build the test of the numbered lines
        go counter (line:rest) = (Just counter, line) : go (counter + 1) rest
    -- call go func, start the recursion by counter = 1 and the list of lines
    in go 1 lines
-}

-- split a string into lines by '\n'
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

isNotEmpty :: String -> Bool
isNotEmpty str = not (isEmpty str)

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber text =
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        go counter (x : xs) = 
            let mNumbering = if shouldNumber x then Just counter else Nothing
                newCounter = if shouldIncr x then counter + 1 else counter
            -- construct a new list by prepending the tuple to the result of the recursive call 'go newCounter xs'
            in (mNumbering, x) : go newCounter xs 
   in go 1 text

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty

data PadMode = PadLeft | PadRight

pad :: PadMode -> Int -> String -> String
pad mode n str =
  let diff = n - length str
      padding = replicate diff ' ' -- create a string of n spaces
   in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode lineNums =
    let (numbers, text) = unzip lineNums
        numberStrings = map (maybe "" show) numbers
        maxLength = maximum (map length numberStrings)
        paddedNumbers = map (pad mode maxLength) numberStrings
    in zipWith (\n l -> n ++ " " ++ l) paddedNumbers text

zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys



    
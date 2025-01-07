module Lib where

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']
upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']
digits :: Alphabet
digits = ['0' .. '9']

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet
isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet
isDigit :: Char -> Bool
isDigit char = char `elem` digits
isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

listLengh [] = 0
listLengh (x : xs) = 1 + listLengh xs

-- return the index of a given char in a given list
indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x : xs) = 
    if x == ch 
        then 0 
        else 1 + indexOf ch xs

-- return a element by its index, !!
index1 :: [a] -> Int -> a
index1 [] _ = undefined
index1 (x : xs) n =
  if n < 0
    then undefined
    else
      if n == 0
        then x
        else index1 xs (n - 1)

upperRot :: Int -> Char -> Char
upperRot n ch = upperAlphabet !! ((indexOf ch upperAlphabet + n) `mod` 26)
lowerRot :: Int -> Char -> Char
lowerRot n ch = lowerAlphabet !! ((indexOf ch lowerAlphabet + n) `mod` 26)

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
  alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | otherwise = ch

caesar :: Int -> String -> String
caesar n message = map (\ch -> rotChar n ch) message

rot13 :: String -> String
rot13 message = caesar 13 message
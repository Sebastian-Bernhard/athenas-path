module Lib where

import Data.Char (toLower)
import Data.List (sortBy)

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
{-
indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x : xs) = 
    if x == ch 
        then 0 
        else 1 + indexOf ch xs
-}

indexOf :: Char -> [Char] -> Maybe Int
indexOf _ [] = Nothing
indexOf ch (x : xs) =
  if x == ch
    then Just 0
    else fmap (+1) (indexOf ch xs)

-- return a element by its index, !!
{-
index1 :: [a] -> Int -> a
index1 [] _ = undefined
index1 (x : xs) n =
  if n < 0
    then undefined
    else
      if n == 0
        then x
        else index1 xs (n - 1)
-}

index1 :: [a] -> Int -> a
index1 [] _ = undefined
index1 (x : xs) n
  | n < 0 = undefined
  | n == 0 = x
  | otherwise = index1 xs (n - 1)

-- Defines a general rotation on an arbitrary alphabet
alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
  -- alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)
  -- using new indexOf function
  case indexOf ch alphabet of
    Just idx -> alphabet !! ((idx + n) `mod` length alphabet)
    Nothing -> ch

-- Defines a rotation function for uppercase letters
upperRot :: Int -> Char -> Char
upperRot n ch = alphabetRot upperAlphabet n ch

-- Defines a rotation function for lowercase letters
lowerRot :: Int -> Char -> Char
lowerRot n ch = alphabetRot lowerAlphabet n ch

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | otherwise = ch

caesar :: Int -> String -> String
caesar n message = map (\ch -> rotChar n ch) message

rot13 :: String -> String
rot13 message = caesar 13 message

count :: Char -> String -> Int
count element [] = 0
count element (x : xs)
  | x == element = 1 + count element xs
  | otherwise = count element xs

frequencyStats :: [Char] -> [(Char, Int)]
frequencyStats xs =
  let input = map toLower xs
      freqs =
        map (\element -> (element, count element input)) lowerAlphabet
   in
      sortBy (\(_, x) (_, y) -> compare y x) freqs

loremIpsum :: String
loremIpsum = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."

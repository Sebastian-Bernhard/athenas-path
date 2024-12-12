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

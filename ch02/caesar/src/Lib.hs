{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- {-# OPTIONS_GHC -Wno-missing-signatures #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

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

indexOf :: Char -> Alphabet -> Int
indexOf _ [] = undefined -- ! Bad style
indexOf c (x : xs) = if x == c then 0 else 1 + indexOf c xs

-- | A general ROT function for any alphabet. Handles cycling around the "wheel".
alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot a n c = a !! ((indexOf c a + n) `mod` length a)

-- >>> upperRot 3 'Z'
-- 'C'
upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

-- >>> lowerRot (-3) 'z'
-- 'w'
lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

-- >>> digitRot 2 '9'
-- '1'
digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

-- >>> rotChar 13 'A'
-- >>> rotChar 13 'a'
-- >>> rotChar 13 '3'
-- 'N'
-- 'n'
-- '6'
rotChar :: Int -> Char -> Char
rotChar n c
  | isLower c = lowerRot n c
  | isUpper c = upperRot n c
  | isDigit c = digitRot n c
  | otherwise = c

-- >>> caesar 13 "Hello, world!"
-- "Uryyb, jbeyq!"
caesar :: Int -> String -> String
caesar n = map (rotChar n)

-- >>> rot13 "Hello, world!"
-- >>> rot13 $ rot13 "Hello, world!"
-- "Uryyb, jbeyq!"
-- "Hello, world!"
rot13 :: String -> String
rot13 = caesar 13

-- >>> rot5 "Hello, world!"
-- "Mjqqt, btwqi!"
rot5 :: String -> String
rot5 = caesar 5

rot135 :: String -> String
rot135 xs = undefined

-- >>> count 'e' "Hello, world!"
-- 1
count :: Char -> String -> Int
count _ [] = 0
count c (x : xs) = (if x == c then 1 else 0) + count c xs

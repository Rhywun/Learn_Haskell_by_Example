module Lib (isEmpty, numberAllLines) where

import Data.Char (isPrint, isSeparator)

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

-- | Prefixes every line in order with a number.
-- >>> numberAllLines ["Hello", "", "World"]
-- [(Just 1,"Hello"),(Just 2,""),(Just 3,"World")]
numberAllLines :: [String] -> NumberedLines
numberAllLines lines' =
  let go _ [] = []
      go counter (x : xs) = (Just counter, x) : go (counter + 1) xs
   in go 1 lines'

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber text =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs) =
        let mNumbering = if shouldNumber x then Just counter else Nothing
            newCounter = if shouldIncr x then counter + 1 else counter
         in (mNumbering, x) : go newCounter xs
   in go 1 text

-- | Checks if a string contains no printable characters.
-- >>> isEmpty "Test"
-- >>> isEmpty "    "
-- >>> isEmpty "\n  "
-- >>> isEmpty "A   "
-- False
-- True
-- True
-- False
isEmpty :: String -> Bool
isEmpty str =
  null str
    || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty str = not (isEmpty str)

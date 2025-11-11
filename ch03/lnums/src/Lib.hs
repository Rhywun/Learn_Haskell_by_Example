module Lib (isEmpty, numberAllLines) where

import Data.Char (isPrint, isSeparator)

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

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

-- | Generalized higher-order function for numbering lines.
numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncrement shouldNumber text =
  let go :: Int -> [String] -> NumberedLines
      go _ [] = []
      go counter (x : xs) =
        let mNumbering = if shouldNumber x then Just counter else Nothing
            newCounter = if shouldIncrement x then counter + 1 else counter
         in (mNumbering, x) : go newCounter xs
   in go 1 text

-- | Numbers every line.
-- >>> numberAllLines ["Hello", "", "world", "!"]
-- [(Just 1,"Hello"),(Just 2,""),(Just 3,"world"),(Just 4,"!")]
numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

-- | Numbers every nonempty line, while incrementing the counter even on empty lines.
-- >>> numberNonEmptyLines ["Hello", "", "world", "!"]
-- [(Just 1,"Hello"),(Nothing,""),(Just 3,"world"),(Just 4,"!")]
numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

-- | Numbers every nonempty line, only incrementing the counter on empty lines.
-- >>> numberAndIncrementNonEmptyLines ["Hello", "", "world", "!"]
-- [(Just 1,"Hello"),(Nothing,""),(Just 2,"world"),(Just 3,"!")]
numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty

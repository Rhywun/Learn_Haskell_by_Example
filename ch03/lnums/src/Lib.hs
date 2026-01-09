module Lib
  ( isEmpty,
    numberAllLines,
    isNotEmpty,
    numberNonEmptyLines,
    numberAndIncrementNonEmptyLines,
    PadMode (..),
    pad,
    padLeft,
    padRight,
    prettyNumberedLines,
  )
where

import Data.Char (isPrint, isSeparator)

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

-- * 4.2 - Parametrized behavior in higher-order functions

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

-- * 4.3 - Algebraic data structures as an encoding of possibilities

data PadMode = PadLeft | PadRight | PadCenter

-- | A generalized function to perform either left or right padding.
-- >>> pad PadLeft 10 "Pad me!"
-- >>> pad PadRight 10 "Pad me!"
-- >>> pad PadCenter 15 "Pad me!"
-- WAS "   Pad me!"
-- WAS "Pad me!   "
-- NOW "   Pad me!"
-- NOW "Pad me!   "
-- NOW "    Pad me!    "
pad :: PadMode -> Int -> String -> String
pad mode n str =
  case mode of
    PadLeft -> padding ++ str
    PadRight -> str ++ padding
    PadCenter -> centerPadding ++ str ++ centerPadding
      where
        centerPadding = replicate (diff `div` 2) ' '
  where
    diff = n - length str
    padding = replicate diff ' '

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

-- | Transform numbered lines into human-readable strings.
-- >>> prettyNumberedLines PadLeft $ numberAllLines ["Hello", "", "world", "!"]
-- ["1 Hello","2 ","3 world","4 !"]
prettyNumberedLines :: PadMode -> NumberedLines -> [String]
prettyNumberedLines mode lineNums =
  zipWith (\n l -> n ++ " " ++ l) paddedNumbers text
  where
    (numbers, text) = unzip lineNums
    numberStrings = map (maybe "" show) numbers
    maxLength = maximum (map length numberStrings)
    paddedNumbers = map (pad mode maxLength) numberStrings

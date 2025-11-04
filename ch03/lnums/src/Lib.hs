module Lib
  (
  )
where

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

numberAllLines :: [String] -> NumberedLines
numberAllLines lines =
  let go _ [] = []
      go counter (x : xs) = (Just counter, x) : go (counter + 1) xs
   in go 1 lines

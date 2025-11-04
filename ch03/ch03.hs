import Data.Maybe

interactiveLines :: Int -> IO ()
interactiveLines n = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn (show n ++ ". " ++ line)
      interactiveLines (n + 1)

-- * Exercise: A safe function for indexing

-- ! No idea how to solve this. My attempt gives the wrong answer.

-- >>> _indexOf 'e' "Hello, world"
-- >>> _indexOf 'a' "Hello, world"
-- 1
-- Prelude.undefined
_indexOf :: Char -> [Char] -> Int
_indexOf _ [] = undefined -- Bad style!
_indexOf c (x : xs) = if x == c then 0 else 1 + _indexOf c xs

-- >>> indexOf 'e' "Hello, world!"
-- >>> indexOf 'a' "Hello, world!"
-- Just 1
-- Just 13
indexOf :: Char -> [Char] -> Maybe Int
indexOf _ [] = Nothing
indexOf c (x : xs) = if x == c then Just 0 else Just (1 + fromMaybe 0 (indexOf c xs))

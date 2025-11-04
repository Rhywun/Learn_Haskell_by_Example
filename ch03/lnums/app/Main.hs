module Main (main) where

import System.Environment

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe
    (printHelpText "Too few or too many args")
    putStrLn
    mFilePath

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn msg
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing

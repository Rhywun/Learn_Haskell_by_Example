module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe
    (printHelpText "Missing filename")
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

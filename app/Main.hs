module Main where

import System.Environment (getArgs, getProgName)
import System.IO (readFile)
import Data.List (intercalate)
import Lexer
import Token

main :: IO ()
main = do
    args <- getArgs
    case args of
      [filename] -> do
          src <- readFile filename
          case tokenise src of
            Left  err    -> putStrLn $ "lexer error: " ++ show err
            Right tokens -> putStrLn $ intercalate "\n" $ map show tokens
      _ -> do
          progName <- getProgName
          putStrLn $ "usage: " ++ progName ++ " <filename>"

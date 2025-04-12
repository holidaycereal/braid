module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Data.List (intercalate)

import Lexer (tokenise, LexError(..))
import Token (Token(..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile filename
      case tokenise ([], source) of
        Left err -> putStrLn $ "lexer error: " ++ show err
        Right tokens -> putStrLn $ intercalate "\n" $ map show tokens
    _ -> putStrLn "usage: ./lexer <filename>"

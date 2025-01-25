module Main (main) where

import Lexer
import System.Environment (getArgs)
import Control.Monad (forM_)

printToken :: Token -> IO ()
printToken (Token typ val) = do
    let typStr = show typ
    let valStr = maybe "" id val
    putStrLn $ typStr ++ " \033[34m" ++ valStr ++ "\033[0m\n"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            input <- readFile filename
            tokens <- Lexer.lex input
            forM_ tokens printToken
        _ -> do
            putStrLn $ "Usage: braid <filename>"

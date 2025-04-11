module Lexer where

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)
import Data.Char (isAlpha, isDigit, isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Control.Monad (join)

type Lexer = ([Token], String)

data LexError = InvalidToken Char | UnexpectedEOF

tokenise :: Lexer -> Either LexError [Token]
tokenise (acc, []) = Right (reverse acc)
tokenise (acc, c : rest)
  | isSpace c = tokenise (acc, rest)
  | isAlpha c || c == '_' = tokenise $ readWord (acc, c : rest)
  | isDigit c = tokenise $ readNumber (acc, c : rest)
  | c == '"' = readTextLiteral (c, StringLiteral) (acc, rest) >>= tokenise
  | c == '\'' = readTextLiteral (c, CharLiteral) (acc, rest) >>= tokenise
  | otherwise = readSymbol (acc, c : rest) >>= \(acc, chars) -> case acc of
      LineComment : tail -> tokenise (tail, dropWhile (/= '\n') chars)
      BlockComment : tail -> skipBlockComment chars >>= \s -> tokenise (tail, s)
      _ -> tokenise (acc, chars)

readWord :: Lexer -> Lexer
readWord (acc, chars) = findKeywordToken keywordTokenDefs chars
  where
    findKeywordToken [] chars =
      let ident = takeWord chars in
      (Identifier ident : acc, drop (length ident) chars)
    findKeywordToken ((word, tok) : tail) chars
      | word == takeWord chars = (tok : acc, drop (length word) chars)
      | otherwise = findKeywordToken tail chars
    takeWord = takeWhile (\c -> isAlphaNum c || c == '_')

readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, chars) = findSymbolToken symbolTokenDefs chars
  where
    findSymbolToken [] (head : _) = Left (InvalidToken head)
    findSymbolToken [] [] = Left UnexpectedEOF
    findSymbolToken ((sym, tok) : tail) chars
      | sym `isPrefixOf` chars = Right (tok : acc, drop (length sym) chars)
      | otherwise = findSymbolToken tail chars

-- TODO: fix
-- prevent multiple decimal points
-- add support for 0b 0x 0o prefixes and scientific notation
readNumber :: Lexer -> Lexer
readNumber (acc, chars) =
  let numStr = takeWhile (\c -> isDigit c || c == '.') chars in
  (NumLiteral numStr : acc, drop (length numStr) chars)

readTextLiteral :: (Char, String -> Token) -> Lexer -> Either LexError Lexer
readTextLiteral (delim, kind) lexer =
  readUntil delim lexer >>= \(s, (acc, rest)) -> Right (kind s : acc, rest)
  where
    readUntil delim (toks, chars) = aux toks "" chars
    aux _ _ [] = Left UnexpectedEOF
    aux toks acc ('\\' : c : rest) = aux toks (c : '\\' : acc) rest
    aux toks acc (c : rest) | c == delim = Right (reverse acc, (toks, acc))
    aux toks acc (c : rest) = aux toks (c : acc) rest

skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 1
  where
    aux [] _ = Left UnexpectedEOF
    aux (_ : rest) 0 = Right rest
    aux ('*' : '-' : rest) depth = aux rest (depth - 1)
    aux ('-' : '*' : rest) depth = aux rest (depth + 1)
    aux (_ : rest) depth = aux rest depth

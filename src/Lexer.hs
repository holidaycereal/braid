module Lexer where

import Data.Char (isAlpha, isDigit, isAlphaNum, isSpace, toLower)
import Data.List (isPrefixOf)
import Control.Monad (join)

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)

type Lexer = ([Token], String)

data LexError = UnexpectedChar Char | UnexpectedEOF
  deriving (Show)

-- main lexing function
tokenise :: Lexer -> Either LexError [Token]

-- lex while there are still characters to consume
tokenise (acc, c:rest)
  | isSpace c = tokenise (acc, dropWhile isSpace rest)
  | isAlpha c || c == '_' = tokenise $ readWord (acc, c:rest)
  | isDigit c = tokenise $ readNumber (acc, c:rest)
  | c == '"' = readTextLiteral (c, StringLiteral) (acc, rest) >>= tokenise
  | c == '\'' = readTextLiteral (c, CharLiteral) (acc, rest) >>= tokenise
  | otherwise = readSymbol (acc, c:rest) >>= tokenise

-- base case: no more characters to consume, return the accumulated tokens
tokenise (acc, []) = Right (reverse acc)

-- read a keyword or identifier
readWord :: Lexer -> Lexer
readWord (acc, chars) = findKeywordToken keywordTokenDefs chars
  where
    -- descend keyword token def list
    findKeywordToken ((word, tok):tail) chars
      | word == takeWhile isIdentChar chars = (tok:acc, drop (length word) chars)
      | otherwise = findKeywordToken tail chars

    -- no match found, read identifier
    findKeywordToken [] chars =
      let (ident, rest) = span isIdentChar chars in (Identifier ident : acc, rest)
    isIdentChar c = isAlphaNum c || c == '_'

-- read a numeric literal
readNumber :: Lexer -> Lexer
readNumber (acc, chars) =
  let
    -- check for binary, octal, or hexadecimal prefix...
    (prefix, rest) = case chars of
      '0':c:tl | toLower c `elem` ['b', 'o', 'x'] -> (['0', c], tl)
      _ -> ("", chars)
    -- ...and define a digit validator function accordingly
    isValidDigit = case prefix of
      ['0', c] | toLower c == 'b' -> (`elem` ['0', '1'])
      ['0', c] | toLower c == 'o' -> (`elem` ['0', '7'])
      ['0', c] | toLower c == 'x' -> \c -> isDigit c || toLower c `elem` ['a'..'f']
      _ -> isDigit

    -- read integral part
    (intPart, afterIntPart) = span isValidDigit rest

    -- for decimal numbers only (no prefix):
    -- read fractional part
    (fracPart, afterFracPart) = if prefix == ""
      then case afterIntPart of
        '.':c:tl | isDigit c ->
          let (part, after) = span isDigit tl in ('.':c:part, after)
        _ -> ("", afterIntPart)
      else ("", afterIntPart)
    -- read exponent part (also only for decimal)
    (expPart, afterNumPart) = if prefix == ""
      then case afterFracPart of
        e:c:tl | toLower e == 'e' && (isDigit c || c `elem` ['-', '+']) ->
          let (part, after) = span isDigit tl in (e:c:part, after)
        _ -> ("", afterFracPart)
      else ("", afterFracPart)

    -- combine everything
    numStr = prefix ++ intPart ++ fracPart ++ expPart
  in
  (NumLiteral numStr : acc, afterNumPart)

-- read a string or char literal
readTextLiteral :: (Char, String -> Token) -> Lexer -> Either LexError Lexer
readTextLiteral (delim, kind) (toks, chars) =
  aux toks "" chars >>= \(s, (acc, rest)) -> Right (kind s : acc, rest)
  where
    aux toks acc ('\\':c:rest) = aux toks (c:'\\':acc) rest
    aux toks acc (c:rest) | c == delim = Right (reverse acc, (toks, rest))
    aux toks acc (c:rest) = aux toks (c:acc) rest
    aux _ _ [] = Left UnexpectedEOF

-- read a 'symbol' (anything that's not alphanumeric or an underscore)
-- this includes comment start tokens, so this function skips comments too
readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, chars) = findSymbolToken symbolTokenDefs chars
  where
    -- descend symbol token def list, error if no match found
    findSymbolToken ((sym, tok):tail) chars
      | sym `isPrefixOf` chars = case tok of
          LineComment -> Right (acc, dropWhile (/= '\n') chars)
          BlockComment -> skipBlockComment chars >>= \s -> Right (acc, s)
          _ -> Right (tok:acc, drop (length sym) chars)
      | otherwise = findSymbolToken tail chars
    findSymbolToken [] chars = Left $ UnexpectedChar $ head chars

-- skip block comments, allowing for nesting
skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 1
  where
    aux ('*':'-':rest) depth = aux rest $ depth - 1
    aux ('-':'*':rest) depth = aux rest $ depth + 1
    aux (_:rest) 0 = Right rest
    aux (_:rest) depth = aux rest depth
    aux [] _ = Left UnexpectedEOF

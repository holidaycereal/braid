module Lexer where

import Data.Char
  (isAlpha, isAlphaNum, isDigit, isOctDigit, isHexDigit, isSpace, toLower)
import Data.List (isPrefixOf)
import Control.Monad (join)

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)

type Lexer = ([Token], String)

data LexError = UnexpectedChar Char | UnterminatedComment | UnterminatedLiteral
  deriving (Show)

-- main lexing function
tokenise :: Lexer -> Either LexError [Token]
tokenise (acc, []) = Right (reverse acc)
tokenise (acc, c:rest)
  | isSpace c = tokenise (acc, dropWhile isSpace rest)
  | isAlpha c || c == '_' = tokenise $ readWord (acc, c:rest)
  | isDigit c = tokenise $ readNumber (acc, c:rest)
  | c == '"' = readTextLiteral (c, StringLiteral) (acc, rest) >>= tokenise
  | c == '\'' = readTextLiteral (c, CharLiteral) (acc, rest) >>= tokenise
  | otherwise = readSymbol (acc, c:rest) >>= tokenise

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
    -- check for 0b/0o/0x prefix and define digit validator function accordingly
    (prefix, rest, isValidDigit) = case chars of
      '0':c:tl | toLower c == 'b' -> (['0', c], tl, (`elem` ['0', '1']))
      '0':c:tl | toLower c == 'o' -> (['0', c], tl, isOctDigit)
      '0':c:tl | toLower c == 'x' -> (['0', c], tl, isHexDigit)
      _ -> ("", chars, isDigit)
    -- read integral part
    (intPart, afterIntPart) = span isValidDigit rest
    -- read fractional part
    (fracPart, afterFracPart) = case (prefix, afterIntPart) of
      ("", '.':c:tl) | isDigit c ->
        (\(part, after) -> ('.':c:part, after)) $ span isDigit tl
      _ -> ("", afterIntPart)
    -- read exponent part
    (expPart, afterNumPart) = case (prefix, afterFracPart) of
      ("", e:c:tl) | toLower e == 'e' && (isDigit c || c `elem` ['-', '+']) ->
        (\(part, after) -> (e:c:part, after)) $ span isDigit tl
      _ -> ("", afterFracPart)
  in
  (NumLiteral (prefix ++ intPart ++ fracPart ++ expPart) : acc, afterNumPart)

-- read a string or char literal
readTextLiteral :: (Char, String -> Token) -> Lexer -> Either LexError Lexer
readTextLiteral (delim, kind) (toks, chars) =
  aux toks "" chars >>= \(s, (acc, rest)) -> Right (kind s : acc, rest)
  where
    aux toks acc (c:rest) | c == delim = Right (reverse acc, (toks, rest))
    aux toks acc ('\\':c:rest) = aux toks (c:'\\':acc) rest
    aux toks acc (c:rest) = aux toks (c:acc) rest
    aux _ _ [] = Left UnterminatedLiteral

-- read a 'symbol' (anything that's not alphanumeric or an underscore)
readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, chars) = findSymbolToken symbolTokenDefs chars
  where
    -- descend symbol token def list
    findSymbolToken ((sym, tok):tail) chars
      | sym `isPrefixOf` chars = case tok of
          -- if it's a comment start token, discard it and skip the comment
          LineComment -> Right (acc, dropWhile (/= '\n') chars)
          BlockComment -> skipBlockComment chars >>= \s -> Right (acc, s)
          -- otherwise, add token to lexer accumulator and return new lexer
          _ -> Right (tok:acc, drop (length sym) chars)
      | otherwise = findSymbolToken tail chars
    -- error if no matching token found
    findSymbolToken [] chars = Left $ UnexpectedChar $ head chars

-- skip block comments, allowing for nesting
skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 0
  where
    aux ('*':'-':rest) depth = aux rest $ depth - 1
    aux ('-':'*':rest) depth = aux rest $ depth + 1
    aux (_:rest) 0 = Right rest
    aux (_:rest) depth = aux rest depth
    aux [] _ = Left UnterminatedComment

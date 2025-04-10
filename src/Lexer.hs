module Lexer where

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)
import Data.Char (isAlpha, isDigit, isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Control.Monad (join)

type Lexer = ([Token], String)

data LexError =
    InvalidToken Char
  | UnterminatedStringLiteral
  | UnterminatedCharLiteral
  | EmptyCharLiteral
  | UnexpectedEOF

tokenise :: Lexer -> Either LexError [Token]
tokenise (acc, []) = Right (reverse acc)
tokenise (acc, c:rest)
  | isSpace c = tokenise (acc, rest)
  | isAlpha c || c == '_' = tokenise $ readWord (acc, c:rest)
  | isDigit c = tokenise $ readNumber (acc, c:rest)
  | c == '"' = readStringLiteral (acc, rest) >>= tokenise
  | c == '\'' = readCharLiteral (acc, rest) >>= tokenise
  | otherwise = readSymbol (acc, c:rest) >>= \(newAcc, chars) ->
      case newAcc of
        LineComment:tail -> tokenise (tail, dropWhile (/= '\n') chars)
        BlockComment:tail -> skipBlockComment chars >>= \s -> tokenise (tail, s)
        _ -> tokenise (newAcc, chars)

skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 1
  where
    aux [] _ = Left UnexpectedEOF
    aux (_:rest) 0 = Right rest
    aux ('*':'-':rest) depth = aux rest (depth - 1)
    aux ('-':'*':rest) depth = aux rest (depth + 1)
    aux (_:rest) depth = aux rest depth

readWord (acc, chars) = findKeywordToken keywordTokenDefs chars
  where
    findKeywordToken [] chars =
      let ident = takeWord chars in
      (Identifier ident : acc, drop (length ident) chars)
    findKeywordToken ((word, tok):tail) chars
      | takeWord chars == word = (tok:acc, drop (length word) chars)
      | otherwise = findKeywordToken tail chars
    takeWord = takeWhile (\c -> isAlphaNum c || c == '_')

readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, chars) = findSymbolToken symbolTokenDefs chars
  where
    findSymbolToken [] (head:_) = Left (InvalidToken head)
    findSymbolToken [] [] = Left UnexpectedEOF
    findSymbolToken ((sym, tok):tail) chars
      | sym `isPrefixOf` chars = Right (tok:acc, drop (length sym) chars)
      | otherwise = findSymbolToken tail chars

-- TODO: fix
-- prevent multiple decimal points
-- add support for 0b 0x 0o prefixes and scientific notation
readNumber :: Lexer -> Lexer
readNumber (acc, chars) =
  let numStr = takeWhile (\c -> isDigit c || c == '.') chars
      rest = drop (length numStr) chars
  in (NumLiteral numStr : acc, rest)

readStringLiteral :: Lexer -> Either LexError Lexer
readStringLiteral (acc, chars) = aux acc "" chars
  where
    aux _ _ [] = Left UnterminatedStringLiteral
    aux acc etc ('"':rest) = Right (StringLiteral (reverse etc) : acc, rest)
    aux acc etc ('\\':c:rest) = aux acc (escChar c : etc) rest
    aux acc etc (c:rest) = aux acc (c : etc) rest

readCharLiteral :: Lexer -> Either LexError Lexer
readCharLiteral (_, []) = Left UnterminatedCharLiteral
readCharLiteral (_, '\'':_) = Left EmptyCharLiteral
readCharLiteral (acc, '\\':c:rest) = case rest of
  '\'':tail -> Right (CharLiteral ['\\', c] : acc, tail)
  _ -> Left UnterminatedCharLiteral
readCharLiteral (acc, c:'\'':rest) = Right (CharLiteral [c] : acc, rest)
readCharLiteral (acc, _) = Left UnterminatedCharLiteral

-- TODO: make this comprehensive
escChar :: Char -> Char
escChar 'n' = '\n'
escChar 't' = '\t'
escChar 'r' = '\r'
escChar '"' = '"'
escChar '\'' = '\''
escChar '\\' = '\\'
escChar c = c

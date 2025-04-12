module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isOctDigit, isHexDigit, isSpace, toLower)
import Data.List (isPrefixOf)
import Control.Monad (join)

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)

type Lexer = ([Token], String)

data LexError = UnexpectedChar Char | UnterminatedComment | UnterminatedTextLiteral
  deriving (Show)

-- main lexing function
tokenise :: Lexer -> Either LexError [Token]
tokenise (acc, []) = Right (reverse acc)
tokenise (acc, c:rest)
  | isSpace c = tokenise (acc, dropWhile isSpace rest)
  | isAlpha c || c == '_' = tokenise $ readWord (acc, c:rest)
  | isDigit c = tokenise $ readNumber (acc, c:rest)
  | c == '"' = readTextLiteral StringLiteral (acc, c:rest) >>= tokenise
  | c == '\'' = readTextLiteral CharLiteral (acc, c:rest) >>= tokenise
  | otherwise = readSymbol (acc, c:rest) >>= tokenise

-- read a keyword or identifier
readWord :: Lexer -> Lexer
readWord (acc, chars) = case findKeyword keywordTokenDefs of
  Just (word, tok) -> (tok:acc, drop (length word) chars)
  Nothing -> (\(s, rest) -> (Identifier s : acc, rest)) $ span isIdentChar chars
  where
    findKeyword ((word, tok):tl)
      | word == takeWhile isIdentChar chars = Just (word, tok)
      | otherwise = findKeyword tl
    findKeyword [] = Nothing
    isIdentChar c = isAlphaNum c || c == '_'

-- read a numeric literal
readNumber :: Lexer -> Lexer
readNumber (acc, chars) =
  let
    (prefix, afterPrefix) = case chars of
      '0':c:tl | toLower c `elem` "box" -> (['0', c], tl)
      _ -> ("", chars)
    (intPart, afterIntPart) = span (isValidDigit prefix) afterPrefix
    (fracPart, afterFracPart) = case (prefix, afterIntPart) of
      ("", '.':c:tl) | isDigit c -> (\(xs, ys) -> ('.':c:xs, ys)) $ span isDigit tl
      _ -> ("", afterIntPart)
    (expPart, afterNum) = case (prefix, afterFracPart) of
      ("", e:c:tl) | toLower e == 'e' && (isDigit c || c `elem` "-+") ->
        (\(xs, ys) -> (e:c:xs, ys)) $ span isDigit tl
      _ -> ("", afterFracPart)
  in
  (NumLiteral (prefix ++ intPart ++ fracPart ++ expPart) : acc, afterNum)
  where
    isValidDigit prefix = case map toLower prefix of
      "0b" -> (`elem` "01")
      "0o" -> isOctDigit
      "0x" -> isHexDigit
      _ -> isDigit

-- read a string or char literal
readTextLiteral :: (String -> Token) -> Lexer -> Either LexError Lexer
readTextLiteral mkToken (toks, chars) =
  aux toks "" (tail chars) >>= \(s, (acc, rest)) -> Right (mkToken s : acc, rest)
  where
    aux toks acc (c:rest) | c == delim = Right (reverse acc, (toks, rest))
    aux toks acc ('\\':c:rest) = aux toks (c:'\\':acc) rest
    aux toks acc (c:rest) = aux toks (c:acc) rest
    aux _ _ [] = Left UnterminatedTextLiteral
    delim = head chars

-- read a 'symbol' (anything that's not alphanumeric or an underscore)
readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, chars) = case findSymbol symbolTokenDefs of
  Nothing -> Left $ UnexpectedChar $ head chars
  Just (_, LineComment) -> Right (acc, dropWhile (/= '\n') chars)
  Just (_, BlockComment) -> skipBlockComment chars >>= \s -> Right (acc, s)
  Just (sym, tok) -> Right (tok:acc, drop (length sym) chars)
  where
    findSymbol ((sym, tok):tl)
      | sym `isPrefixOf` chars = Just (sym, tok)
      | otherwise = findSymbol tl
    findSymbol [] = Nothing

-- skip block comments, allowing for nesting
skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 0
  where
    aux ('*':'-':rest) depth = aux rest (depth - 1)
    aux ('-':'*':rest) depth = aux rest (depth + 1)
    aux (_:rest) 0 = Right rest
    aux (_:rest) depth = aux rest depth
    aux [] _ = Left UnterminatedComment

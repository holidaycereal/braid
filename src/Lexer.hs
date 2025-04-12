module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isOctDigit, isHexDigit, isSpace, toLower)
import Data.List (isPrefixOf)
import Control.Monad (join)

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)

type Lexer = ([Token], String)

data LexError =
    UnexpectedChar Char
  | UnterminatedComment
  | UnterminatedTextLiteral
  deriving (Show)

-- main lexing function
tokenise :: Lexer -> Either LexError [Token]
tokenise (acc, []) = Right (reverse acc)
tokenise (acc, c:rest)
  | isSpace c             = tokenise (acc, dropWhile isSpace rest)
  | isAlpha c || c == '_' = tokenise $ readWord   (acc, c:rest)
  | isDigit c             = tokenise $ readNumber (acc, c:rest)
  | c == '"'  = readTextLiteral StringLiteral (acc, c:rest) >>= tokenise
  | c == '\'' = readTextLiteral CharLiteral   (acc, c:rest) >>= tokenise
  | otherwise = readSymbol                    (acc, c:rest) >>= tokenise

-- read a keyword or identifier
readWord :: Lexer -> Lexer
readWord (acc, cs) =
  case findKeyword keywordTokenDefs of
    Just (kw, tok) -> (tok:acc, drop (length kw) cs)
    Nothing        -> let (id, rest) = span isIdentChar cs
                      in (Identifier id : acc, rest)
  where
    findKeyword ((kw, tok):tl) | kw == takeWord cs = Just (kw, tok)
                               | otherwise         = findKeyword tl
    findKeyword [] = Nothing

    isIdentChar c = isAlphaNum c || c == '_'
    takeWord = takeWhile isIdentChar

-- read a numeric literal
readNumber :: Lexer -> Lexer
readNumber (acc, cs) =
  let
    (prefix, afterPrefix) = case cs of
      '0':c:tl | toLower c `elem` "box" -> (['0', c], tl)
      _                                 -> ("", cs)

    (intPart, afterIntPart) = span (isValidDigit prefix) afterPrefix

    (fracPart, afterFracPart) = case (prefix, afterIntPart) of
      ("", '.':d:tl)      | isDigit d -> let (ds, rest) = span isDigit tl
                                         in ('.':d:ds, rest)
      _ -> ("", afterIntPart)

    (expPart, afterNum) = case (prefix, afterFracPart) of
      ("", exp:sign:d:tl) | toLower exp == 'e' &&
                            sign `elem` "-+" &&
                            isDigit d -> let (ds, rest) = span isDigit tl
                                         in (exp:sign:d:ds, rest)
      ("", exp:d:tl)      | toLower exp == 'e' &&
                            isDigit d -> let (ds, rest) = span isDigit tl
                                         in (exp:d:ds, rest)
      _ -> ("", afterFracPart)
  in
    (NumLiteral (prefix ++ intPart ++ fracPart ++ expPart) : acc, afterNum)
  where
    isValidDigit prefix = case map toLower prefix of
      "0b" -> (`elem` "01")
      "0o" -> isOctDigit
      "0x" -> isHexDigit
      _    -> isDigit

-- read a string or char literal
readTextLiteral :: (String -> Token) -> Lexer -> Either LexError Lexer
readTextLiteral mkToken (acc, cs) =
    aux "" (tail cs) >>= \(str, cs) -> Right (mkToken str : acc, cs)
  where
    aux str ('\\':c:rest)           = aux (c:'\\':str) rest
    aux str (c:rest) | c == head cs = Right (reverse str, rest)
                     | otherwise    = aux (c:str) rest
    aux _ [] = Left UnterminatedTextLiteral

-- read a 'symbol' (anything that's not alphanumeric or an underscore)
readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, cs) =
  case findSymbol symbolTokenDefs of
    Just (_, LineComment)  -> Right (acc, dropWhile (/= '\n') cs)
    Just (_, BlockComment) -> skipBlockComment cs >>= \cs -> Right (acc, cs)
    Just (sym, tok)        -> Right (tok:acc, drop (length sym) cs)
    Nothing -> Left $ UnexpectedChar $ head cs
  where
    findSymbol ((sym, tok):tl) | sym `isPrefixOf` cs = Just (sym, tok)
                               | otherwise           = findSymbol tl
    findSymbol [] = Nothing

-- skip block comments, allowing for nesting
skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 0
  where
    aux ('*':'-':rest) n = aux rest (n - 1)
    aux ('-':'*':rest) n = aux rest (n + 1)
    aux (_      :rest) n = if n < 1 then Right rest else aux rest n
    aux [] _ = Left UnterminatedComment

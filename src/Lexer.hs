module Lexer where

import Data.Char (isAlpha, isDigit, isOctDigit, isHexDigit, isSpace, toLower)
import Data.List (isPrefixOf)
import Control.Monad ((>>=))

import Token (Token(..), keywordTokenDefs, symbolTokenDefs)

type Lexer = ([Token], String)

data LexError = Unknown Char | UnclosedComment | UnclosedLiteral deriving Show

-- main lexing function
tokenise :: Lexer -> Either LexError [Token]
tokenise (acc, []) = Right $ reverse acc
tokenise (acc, c:rest)
  | isSpace c             = tokenise (acc, dropWhile isSpace rest)
  | isAlpha c || c == '_' = tokenise $ readWord   (acc, c:rest)
  | isDigit c             = tokenise $ readNumber (acc, c:rest)
  | c == '"'  = readTextLiteral StringLiteral (acc, c:rest) >>= tokenise
  | c == '\'' = readTextLiteral CharLiteral   (acc, c:rest) >>= tokenise
  | otherwise = readSymbol (acc, c:rest) >>= tokenise

-- read a keyword or identifier
readWord :: Lexer -> Lexer
readWord (acc, cs) = case findKeyword keywordTokenDefs of
    Just (kw, tok) -> (tok:acc, drop (length kw) cs)
    Nothing -> let (id, rs) = span isIdentChar cs in (Identifier id : acc, rs)
  where
    findKeyword ((kw, tok):tl)
      | kw == takeWhile isIdentChar cs = Just (kw, tok)
      | otherwise                      = findKeyword tl
    findKeyword []                     = Nothing
    isIdentChar c = isAlpha c || isDigit c || c == '_'

-- read a numeric literal
readNumber :: Lexer -> Lexer
readNumber (acc, cs) =
    let
      (isBase10, isValidDigit) = case cs of
          '0':c:d:_ | isDigit d -> case toLower c of
                                     'b' -> (False, (`elem` "01"))
                                     'o' -> (False, isOctDigit)
                                     'x' -> (False, isHexDigit)
                                     _   -> (True,  isDigit)
          _ -> (True, isDigit)

      (intPart, afterInt) = span isValidDigit $ if isBase10 then cs else drop 2 cs

      (fracPart, afterFrac) = case (isBase10, afterInt) of
          (True, '.':d:tl) | isDigit d ->
              let (ds, rs) = span isDigit tl in ('.':d:ds, rs)
          _ -> ("", afterInt)

      (expPart, afterNum) = case (isBase10, afterFrac) of
          (True, e:sign:d:tl) | e `elem` "Ee" && sign `elem` "-+" && isDigit d ->
              let (ds, rs) = span isDigit tl in (e:sign:d:ds, rs)
          (True, e:d:tl)      | e `elem` "Ee" && isDigit d ->
              let (ds, rs) = span isDigit tl in (e:d:ds, rs)
          _ -> ("", afterFrac)
    in
    (NumLiteral (intPart ++ fracPart ++ expPart) : acc, afterNum)

-- read a string or char literal
readTextLiteral :: (String -> Token) -> Lexer -> Either LexError Lexer
readTextLiteral mkToken (acc, cs) =
    aux "" (tail cs) >>= \(str, cs) -> Right (mkToken str : acc, cs)
  where
    aux str ('\\':c:rest)           = aux (c:'\\':str) rest
    aux str (c:rest) | c == head cs = Right (reverse str, rest)
                     | otherwise    = aux (c:str) rest
    aux _   []                      = Left UnclosedLiteral

-- read a 'symbol' (anything that's not alphanumeric or an underscore)
readSymbol :: Lexer -> Either LexError Lexer
readSymbol (acc, cs) = case findSymbol symbolTokenDefs of
    Just (_, LineComment)  -> Right (acc, dropWhile (/= '\n') cs)
    Just (_, BlockComment) -> skipBlockComment cs >>= \cs -> Right (acc, cs)
    Just (sym, tok)        -> Right (tok:acc, drop (length sym) cs)
    Nothing                -> Left $ Unknown $ head cs
  where
    findSymbol ((sym, tok):tl)
      | sym `isPrefixOf` cs = Just (sym, tok)
      | otherwise           = findSymbol tl
    findSymbol []           = Nothing

-- skip block comments, allowing for nesting
skipBlockComment :: String -> Either LexError String
skipBlockComment s = aux s 0 where
    aux ('*':'-':rest) n = aux rest (n - 1)
    aux ('-':'*':rest) n = aux rest (n + 1)
    aux (_      :rest) n = if n < 1 then Right rest else aux rest n
    aux []             _ = Left UnclosedComment

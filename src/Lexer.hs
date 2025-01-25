{-# LANGUAGE ForeignFunctionInterface #-}
module Lexer where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Control.Monad
import Data.Function (fix)

data TokenType =
-- {{{
      TokWordIf
    | TokWordThen
    | TokWordElse
    | TokWordElif
    | TokWordWhile
    | TokWordFor
    | TokWordIn
    | TokWordBreak
    | TokWordContinue
    | TokWordMatch
    | TokWordWith
    | TokWordSwitch
    | TokWordDyn
    | TokWordType
    | TokWordRecord
    | TokWordInclude
    | TokWordImport
    | TokWordExit
    | TokWordAnd
    | TokWordOr
    | TokWordXor
    | TokWordNot
    | TokPrimU8
    | TokPrimU16
    | TokPrimU32
    | TokPrimU64
    | TokPrimI8
    | TokPrimI16
    | TokPrimI32
    | TokPrimI64
    | TokPrimUsize
    | TokPrimIsize
    | TokPrimF32
    | TokPrimF64
    | TokPrimBool
    | TokLitTrue
    | TokLitFalse
    | TokIdent
    | TokLitIntDec
    | TokLitIntHex
    | TokLitIntOct
    | TokLitIntBin
    | TokLitFloat
    | TokLitChar
    | TokLitStr
    | TokLitStrRaw
    | TokErr
    | TokParenL
    | TokParenR
    | TokBracketL
    | TokBracketR
    | TokBraceL
    | TokBraceR
    | TokDot
    | TokComma
    | TokSemicolon
    | TokColon
    | TokEquals
    | TokVertLine
    | TokAmpersand
    | TokBang
    | TokQuestion
    | TokCaret
    | TokMinus
    | TokPlus
    | TokStar
    | TokSlash
    | TokPercent
    | TokLess
    | TokGreater
    | TokCompEq
    | TokCompNe
    | TokCompLe
    | TokCompGe
    | TokArrow
    | TokReturnArrow
    | TokFwdCompose
    | TokPlusEquals
    | TokMinusEquals
    | TokStarEquals
    | TokSlashEquals
    | TokCaretEquals
    | TokPercentEquals
    | TokRange
    | TokModule
    | TokEof
    deriving (Show, Eq)
-- }}}

data Token = Token
    { tokenType :: TokenType
    , tokenValue :: Maybe String
    } deriving (Show)

data CToken = CToken
    { cTokenType :: CInt
    , cTokenValue :: CString
    }

-- FFI declaration
foreign import ccall "get_token_size" c_token_size :: CSize
foreign import ccall "get_token_alignment" c_token_alignment :: CSize
foreign import ccall "get_token_type_size" c_token_type_size :: CSize
foreign import ccall "lex" c_lex :: CString -> IO (Ptr CToken)
foreign import ccall "free_tokens" c_free_tokens :: Ptr CToken -> IO ()

-- Defining the implementation for Storable
instance Storable CToken where
    sizeOf _ = fromIntegral c_token_size
    alignment _ = fromIntegral c_token_alignment
    peek ptr = do
        typ <- peekByteOff ptr 0  -- first field, zero offset from struct ptr
        val <- peekByteOff ptr (fromIntegral c_token_type_size)
        return $ CToken typ val
    poke = undefined

-- Convert C tokens to Haskell tokens
convertCTokenType :: CInt -> TokenType
-- {{{
convertCTokenType 0 = TokWordIf
convertCTokenType 1 = TokWordThen
convertCTokenType 2 = TokWordElse
convertCTokenType 3 = TokWordElif
convertCTokenType 4 = TokWordWhile
convertCTokenType 5 = TokWordFor
convertCTokenType 6 = TokWordIn
convertCTokenType 7 = TokWordBreak
convertCTokenType 8 = TokWordContinue
convertCTokenType 9 = TokWordMatch
convertCTokenType 10 = TokWordWith
convertCTokenType 11 = TokWordSwitch
convertCTokenType 12 = TokWordDyn
convertCTokenType 13 = TokWordType
convertCTokenType 14 = TokWordRecord
convertCTokenType 15 = TokWordInclude
convertCTokenType 16 = TokWordImport
convertCTokenType 17 = TokWordExit
convertCTokenType 18 = TokWordAnd
convertCTokenType 19 = TokWordOr
convertCTokenType 20 = TokWordXor
convertCTokenType 21 = TokWordNot
convertCTokenType 22 = TokPrimU8
convertCTokenType 23 = TokPrimU16
convertCTokenType 24 = TokPrimU32
convertCTokenType 25 = TokPrimU64
convertCTokenType 26 = TokPrimI8
convertCTokenType 27 = TokPrimI16
convertCTokenType 28 = TokPrimI32
convertCTokenType 29 = TokPrimI64
convertCTokenType 30 = TokPrimUsize
convertCTokenType 31 = TokPrimIsize
convertCTokenType 32 = TokPrimF32
convertCTokenType 33 = TokPrimF64
convertCTokenType 34 = TokPrimBool
convertCTokenType 35 = TokLitTrue
convertCTokenType 36 = TokLitFalse
convertCTokenType 37 = TokIdent
convertCTokenType 38 = TokLitIntDec
convertCTokenType 39 = TokLitIntHex
convertCTokenType 40 = TokLitIntOct
convertCTokenType 41 = TokLitIntBin
convertCTokenType 42 = TokLitFloat
convertCTokenType 43 = TokLitChar
convertCTokenType 44 = TokLitStr
convertCTokenType 45 = TokLitStrRaw
convertCTokenType 46 = TokErr
convertCTokenType 47 = TokParenL
convertCTokenType 48 = TokParenR
convertCTokenType 49 = TokBracketL
convertCTokenType 50 = TokBracketR
convertCTokenType 51 = TokBraceL
convertCTokenType 52 = TokBraceR
convertCTokenType 53 = TokDot
convertCTokenType 54 = TokComma
convertCTokenType 55 = TokSemicolon
convertCTokenType 56 = TokColon
convertCTokenType 57 = TokEquals
convertCTokenType 58 = TokVertLine
convertCTokenType 59 = TokAmpersand
convertCTokenType 60 = TokBang
convertCTokenType 61 = TokQuestion
convertCTokenType 62 = TokCaret
convertCTokenType 63 = TokMinus
convertCTokenType 64 = TokPlus
convertCTokenType 65 = TokStar
convertCTokenType 66 = TokSlash
convertCTokenType 67 = TokPercent
convertCTokenType 68 = TokLess
convertCTokenType 69 = TokGreater
convertCTokenType 70 = TokCompEq
convertCTokenType 71 = TokCompNe
convertCTokenType 72 = TokCompLe
convertCTokenType 73 = TokCompGe
convertCTokenType 74 = TokArrow
convertCTokenType 75 = TokReturnArrow
convertCTokenType 76 = TokFwdCompose
convertCTokenType 77 = TokPlusEquals
convertCTokenType 78 = TokMinusEquals
convertCTokenType 79 = TokStarEquals
convertCTokenType 80 = TokSlashEquals
convertCTokenType 81 = TokCaretEquals
convertCTokenType 82 = TokPercentEquals
convertCTokenType 83 = TokRange
convertCTokenType 84 = TokModule
convertCTokenType 85 = TokEof
-- }}}

convertCToken :: CToken -> IO Token
convertCToken (CToken cTyp cVal) = do
    val <- if cVal == nullPtr
        then return Nothing
        else Just <$> peekCString cVal
    return $ Token (convertCTokenType cTyp) val

-- Helper function to read the token array
readTokens :: Ptr CToken -> Int -> IO [Token]
readTokens ptr idx = do
    cToken <- peekElemOff ptr idx
    token <- convertCToken cToken
    case tokenType token of
        TokEof -> return [token]
        _ -> (token :) <$> readTokens ptr (idx + 1)

-- Wrapper for the lex function
lex :: String -> IO [Token]
lex input = do
    withCString input $ \cStr -> do
        tokenPtr <- c_lex cStr
        tokens <- readTokens tokenPtr 0
        c_free_tokens tokenPtr
        return tokens

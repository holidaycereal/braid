type token_type =
(* GENERATE BEGIN TYPE {{{ *)
  | TokWordIf
  | TokWordElse
  | TokWordElif
  | TokWordWhile
  | TokWordFor
  | TokWordDo
  | TokWordIn
  | TokWordBreak
  | TokWordContinue
  | TokWordMatch
  | TokWordWith
  | TokWordWhen
  | TokWordSwitch
  | TokWordCase
  | TokWordDefault
  | TokWordType
  | TokWordRecord
  | TokWordConst
  | TokWordAlias
  | TokWordFn
  | TokWordInclude
  | TokWordImport
  | TokWordAs
  | TokWordExit
  | TokWordAnd
  | TokWordOr
  | TokWordXor
  | TokPrimByte
  | TokPrimUint
  | TokPrimU8
  | TokPrimU16
  | TokPrimU32
  | TokPrimU64
  | TokPrimInt
  | TokPrimI8
  | TokPrimI16
  | TokPrimI32
  | TokPrimI64
  | TokPrimUsize
  | TokPrimIsize
  | TokPrimF32
  | TokPrimF64
  | TokPrimFloat
  | TokPrimDyn
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
  | TokEof
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
  | TokHash
  | TokTilde
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
  | TokRange
  | TokModule
  | TokConcat
  | TokBitLsl
  | TokBitLsr
  | TokBitAsl
  | TokBitAsr
(* GENERATE END TYPE }}} *)

type token = {
  token_type : token_type;
  token_value : string option;
}

external c_lex : string -> (int * string) array = "c_lex"

let convert_token_type = function
(* GENERATE BEGIN CONVERT {{{ *)
  | 0 -> TokWordIf
  | 1 -> TokWordElse
  | 2 -> TokWordElif
  | 3 -> TokWordWhile
  | 4 -> TokWordFor
  | 5 -> TokWordDo
  | 6 -> TokWordIn
  | 7 -> TokWordBreak
  | 8 -> TokWordContinue
  | 9 -> TokWordMatch
  | 10 -> TokWordWith
  | 11 -> TokWordWhen
  | 12 -> TokWordSwitch
  | 13 -> TokWordCase
  | 14 -> TokWordDefault
  | 15 -> TokWordType
  | 16 -> TokWordRecord
  | 17 -> TokWordConst
  | 18 -> TokWordAlias
  | 19 -> TokWordFn
  | 20 -> TokWordInclude
  | 21 -> TokWordImport
  | 22 -> TokWordAs
  | 23 -> TokWordExit
  | 24 -> TokWordAnd
  | 25 -> TokWordOr
  | 26 -> TokWordXor
  | 27 -> TokPrimByte
  | 28 -> TokPrimUint
  | 29 -> TokPrimU8
  | 30 -> TokPrimU16
  | 31 -> TokPrimU32
  | 32 -> TokPrimU64
  | 33 -> TokPrimInt
  | 34 -> TokPrimI8
  | 35 -> TokPrimI16
  | 36 -> TokPrimI32
  | 37 -> TokPrimI64
  | 38 -> TokPrimUsize
  | 39 -> TokPrimIsize
  | 40 -> TokPrimF32
  | 41 -> TokPrimF64
  | 42 -> TokPrimFloat
  | 43 -> TokPrimDyn
  | 44 -> TokPrimBool
  | 45 -> TokLitTrue
  | 46 -> TokLitFalse
  | 47 -> TokIdent
  | 48 -> TokLitIntDec
  | 49 -> TokLitIntHex
  | 50 -> TokLitIntOct
  | 51 -> TokLitIntBin
  | 52 -> TokLitFloat
  | 53 -> TokLitChar
  | 54 -> TokLitStr
  | 55 -> TokLitStrRaw
  | 56 -> TokErr
  | 57 -> TokEof
  | 58 -> TokParenL
  | 59 -> TokParenR
  | 60 -> TokBracketL
  | 61 -> TokBracketR
  | 62 -> TokBraceL
  | 63 -> TokBraceR
  | 64 -> TokDot
  | 65 -> TokComma
  | 66 -> TokSemicolon
  | 67 -> TokColon
  | 68 -> TokEquals
  | 69 -> TokVertLine
  | 70 -> TokAmpersand
  | 71 -> TokHash
  | 72 -> TokTilde
  | 73 -> TokBang
  | 74 -> TokQuestion
  | 75 -> TokCaret
  | 76 -> TokMinus
  | 77 -> TokPlus
  | 78 -> TokStar
  | 79 -> TokSlash
  | 80 -> TokPercent
  | 81 -> TokLess
  | 82 -> TokGreater
  | 83 -> TokCompEq
  | 84 -> TokCompNe
  | 85 -> TokCompLe
  | 86 -> TokCompGe
  | 87 -> TokArrow
  | 88 -> TokReturnArrow
  | 89 -> TokFwdCompose
  | 90 -> TokRange
  | 91 -> TokModule
  | 92 -> TokConcat
  | 93 -> TokBitLsl
  | 94 -> TokBitLsr
  | 95 -> TokBitAsl
  | 96 -> TokBitAsr
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token (ttyp, tval) = {
  token_type = convert_token_type ttyp;
  token_value = if tval = "" then None else Some tval;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

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
  | TokWordNot
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
  | 27 -> TokWordNot
  | 28 -> TokPrimByte
  | 29 -> TokPrimUint
  | 30 -> TokPrimU8
  | 31 -> TokPrimU16
  | 32 -> TokPrimU32
  | 33 -> TokPrimU64
  | 34 -> TokPrimInt
  | 35 -> TokPrimI8
  | 36 -> TokPrimI16
  | 37 -> TokPrimI32
  | 38 -> TokPrimI64
  | 39 -> TokPrimUsize
  | 40 -> TokPrimIsize
  | 41 -> TokPrimF32
  | 42 -> TokPrimF64
  | 43 -> TokPrimFloat
  | 44 -> TokPrimDyn
  | 45 -> TokPrimBool
  | 46 -> TokLitTrue
  | 47 -> TokLitFalse
  | 48 -> TokIdent
  | 49 -> TokLitIntDec
  | 50 -> TokLitIntHex
  | 51 -> TokLitIntOct
  | 52 -> TokLitIntBin
  | 53 -> TokLitFloat
  | 54 -> TokLitChar
  | 55 -> TokLitStr
  | 56 -> TokLitStrRaw
  | 57 -> TokErr
  | 58 -> TokEof
  | 59 -> TokParenL
  | 60 -> TokParenR
  | 61 -> TokBracketL
  | 62 -> TokBracketR
  | 63 -> TokBraceL
  | 64 -> TokBraceR
  | 65 -> TokDot
  | 66 -> TokComma
  | 67 -> TokSemicolon
  | 68 -> TokColon
  | 69 -> TokEquals
  | 70 -> TokVertLine
  | 71 -> TokAmpersand
  | 72 -> TokHash
  | 73 -> TokTilde
  | 74 -> TokBang
  | 75 -> TokQuestion
  | 76 -> TokCaret
  | 77 -> TokMinus
  | 78 -> TokPlus
  | 79 -> TokStar
  | 80 -> TokSlash
  | 81 -> TokPercent
  | 82 -> TokLess
  | 83 -> TokGreater
  | 84 -> TokCompEq
  | 85 -> TokCompNe
  | 86 -> TokCompLe
  | 87 -> TokCompGe
  | 88 -> TokArrow
  | 89 -> TokReturnArrow
  | 90 -> TokFwdCompose
  | 91 -> TokRange
  | 92 -> TokModule
  | 93 -> TokConcat
  | 94 -> TokBitLsl
  | 95 -> TokBitLsr
  | 96 -> TokBitAsl
  | 97 -> TokBitAsr
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token : int * string -> token = fun (c_typ, c_val) -> {
  token_type = convert_token_type c_typ;
  token_value = if c_val = "" then None else Some c_val;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

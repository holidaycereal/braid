type token_type =
(* GENERATE BEGIN TYPE {{{ *)
  | TokWordIf
  | TokWordWith
  | TokWordElse
  | TokWordWhile
  | TokWordFor
  | TokWordIn
  | TokWordBreak
  | TokWordContinue
  | TokWordMatch
  | TokWordWhen
  | TokWordSwitch
  | TokWordCase
  | TokWordDefault
  | TokWordImmut
  | TokWordType
  | TokWordAlias
  | TokWordRecord
  | TokWordUnion
  | TokWordFn
  | TokWordImport
  | TokWordAs
  | TokWordFrom
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
  | TokPrimUint
  | TokPrimInt
  | TokPrimUsize
  | TokPrimIsize
  | TokPrimF32
  | TokPrimF64
  | TokPrimFloat
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
  | TokAddAssign
  | TokSubAssign
  | TokMulAssign
  | TokDivAssign
  | TokModAssign
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
  | TokBitAnd
  | TokBitOr
  | TokBitXor
(* GENERATE END TYPE }}} *)

type token = {
  token_type : token_type;
  token_value : string option;
}

external c_lex : string -> (int * string) array = "c_lex"

let convert_token_type = function
(* GENERATE BEGIN CONVERT {{{ *)
  | 0 -> TokWordIf
  | 1 -> TokWordWith
  | 2 -> TokWordElse
  | 3 -> TokWordWhile
  | 4 -> TokWordFor
  | 5 -> TokWordIn
  | 6 -> TokWordBreak
  | 7 -> TokWordContinue
  | 8 -> TokWordMatch
  | 9 -> TokWordWhen
  | 10 -> TokWordSwitch
  | 11 -> TokWordCase
  | 12 -> TokWordDefault
  | 13 -> TokWordImmut
  | 14 -> TokWordType
  | 15 -> TokWordAlias
  | 16 -> TokWordRecord
  | 17 -> TokWordUnion
  | 18 -> TokWordFn
  | 19 -> TokWordImport
  | 20 -> TokWordAs
  | 21 -> TokWordFrom
  | 22 -> TokWordAnd
  | 23 -> TokWordOr
  | 24 -> TokWordXor
  | 25 -> TokWordNot
  | 26 -> TokPrimU8
  | 27 -> TokPrimU16
  | 28 -> TokPrimU32
  | 29 -> TokPrimU64
  | 30 -> TokPrimI8
  | 31 -> TokPrimI16
  | 32 -> TokPrimI32
  | 33 -> TokPrimI64
  | 34 -> TokPrimUint
  | 35 -> TokPrimInt
  | 36 -> TokPrimUsize
  | 37 -> TokPrimIsize
  | 38 -> TokPrimF32
  | 39 -> TokPrimF64
  | 40 -> TokPrimFloat
  | 41 -> TokPrimBool
  | 42 -> TokLitTrue
  | 43 -> TokLitFalse
  | 44 -> TokIdent
  | 45 -> TokLitIntDec
  | 46 -> TokLitIntHex
  | 47 -> TokLitIntOct
  | 48 -> TokLitIntBin
  | 49 -> TokLitFloat
  | 50 -> TokLitChar
  | 51 -> TokLitStr
  | 52 -> TokLitStrRaw
  | 53 -> TokErr
  | 54 -> TokEof
  | 55 -> TokParenL
  | 56 -> TokParenR
  | 57 -> TokBracketL
  | 58 -> TokBracketR
  | 59 -> TokBraceL
  | 60 -> TokBraceR
  | 61 -> TokDot
  | 62 -> TokComma
  | 63 -> TokSemicolon
  | 64 -> TokColon
  | 65 -> TokEquals
  | 66 -> TokVertLine
  | 67 -> TokAmpersand
  | 68 -> TokTilde
  | 69 -> TokBang
  | 70 -> TokQuestion
  | 71 -> TokCaret
  | 72 -> TokMinus
  | 73 -> TokPlus
  | 74 -> TokStar
  | 75 -> TokSlash
  | 76 -> TokPercent
  | 77 -> TokLess
  | 78 -> TokGreater
  | 79 -> TokCompEq
  | 80 -> TokCompNe
  | 81 -> TokCompLe
  | 82 -> TokCompGe
  | 83 -> TokAddAssign
  | 84 -> TokSubAssign
  | 85 -> TokMulAssign
  | 86 -> TokDivAssign
  | 87 -> TokModAssign
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
  | 98 -> TokBitAnd
  | 99 -> TokBitOr
  | 100 -> TokBitXor
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token : int * string -> token = fun (c_typ, c_val) -> {
  token_type = convert_token_type c_typ;
  token_value = if c_val = "" then None else Some c_val;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

type token_type =
(* GENERATE BEGIN TYPE {{{ *)
  | TokWordIf
  | TokWordElse
  | TokWordElif
  | TokWordWhile
  | TokWordFor
  | TokWordIn
  | TokWordTo
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
  | 1 -> TokWordElse
  | 2 -> TokWordElif
  | 3 -> TokWordWhile
  | 4 -> TokWordFor
  | 5 -> TokWordIn
  | 6 -> TokWordTo
  | 7 -> TokWordBreak
  | 8 -> TokWordContinue
  | 9 -> TokWordMatch
  | 10 -> TokWordWhen
  | 11 -> TokWordSwitch
  | 12 -> TokWordCase
  | 13 -> TokWordDefault
  | 14 -> TokWordImmut
  | 15 -> TokWordType
  | 16 -> TokWordAlias
  | 17 -> TokWordRecord
  | 18 -> TokWordUnion
  | 19 -> TokWordFn
  | 20 -> TokWordImport
  | 21 -> TokWordAs
  | 22 -> TokWordFrom
  | 23 -> TokWordAnd
  | 24 -> TokWordOr
  | 25 -> TokWordXor
  | 26 -> TokWordNot
  | 27 -> TokPrimU8
  | 28 -> TokPrimU16
  | 29 -> TokPrimU32
  | 30 -> TokPrimU64
  | 31 -> TokPrimI8
  | 32 -> TokPrimI16
  | 33 -> TokPrimI32
  | 34 -> TokPrimI64
  | 35 -> TokPrimUint
  | 36 -> TokPrimInt
  | 37 -> TokPrimUsize
  | 38 -> TokPrimIsize
  | 39 -> TokPrimF32
  | 40 -> TokPrimF64
  | 41 -> TokPrimFloat
  | 42 -> TokPrimBool
  | 43 -> TokLitTrue
  | 44 -> TokLitFalse
  | 45 -> TokIdent
  | 46 -> TokLitIntDec
  | 47 -> TokLitIntHex
  | 48 -> TokLitIntOct
  | 49 -> TokLitIntBin
  | 50 -> TokLitFloat
  | 51 -> TokLitChar
  | 52 -> TokLitStr
  | 53 -> TokLitStrRaw
  | 54 -> TokErr
  | 55 -> TokEof
  | 56 -> TokParenL
  | 57 -> TokParenR
  | 58 -> TokBracketL
  | 59 -> TokBracketR
  | 60 -> TokBraceL
  | 61 -> TokBraceR
  | 62 -> TokDot
  | 63 -> TokComma
  | 64 -> TokSemicolon
  | 65 -> TokColon
  | 66 -> TokEquals
  | 67 -> TokVertLine
  | 68 -> TokAmpersand
  | 69 -> TokTilde
  | 70 -> TokBang
  | 71 -> TokQuestion
  | 72 -> TokCaret
  | 73 -> TokMinus
  | 74 -> TokPlus
  | 75 -> TokStar
  | 76 -> TokSlash
  | 77 -> TokPercent
  | 78 -> TokLess
  | 79 -> TokGreater
  | 80 -> TokCompEq
  | 81 -> TokCompNe
  | 82 -> TokCompLe
  | 83 -> TokCompGe
  | 84 -> TokAddAssign
  | 85 -> TokSubAssign
  | 86 -> TokMulAssign
  | 87 -> TokDivAssign
  | 88 -> TokModAssign
  | 89 -> TokArrow
  | 90 -> TokReturnArrow
  | 91 -> TokFwdCompose
  | 92 -> TokRange
  | 93 -> TokModule
  | 94 -> TokConcat
  | 95 -> TokBitLsl
  | 96 -> TokBitLsr
  | 97 -> TokBitAsl
  | 98 -> TokBitAsr
  | 99 -> TokBitAnd
  | 100 -> TokBitOr
  | 101 -> TokBitXor
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token : int * string -> token = fun (c_typ, c_val) -> {
  token_type = convert_token_type c_typ;
  token_value = if c_val = "" then None else Some c_val;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

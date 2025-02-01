type token_type =
(* GENERATE BEGIN TYPE {{{ *)
  | TokWordIf
  | TokWordElse
  | TokWordElif
  | TokWordWhile
  | TokWordFor
  | TokWordDo
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
  | 5 -> TokWordDo
  | 6 -> TokWordIn
  | 7 -> TokWordTo
  | 8 -> TokWordBreak
  | 9 -> TokWordContinue
  | 10 -> TokWordMatch
  | 11 -> TokWordWhen
  | 12 -> TokWordSwitch
  | 13 -> TokWordCase
  | 14 -> TokWordDefault
  | 15 -> TokWordImmut
  | 16 -> TokWordType
  | 17 -> TokWordAlias
  | 18 -> TokWordRecord
  | 19 -> TokWordUnion
  | 20 -> TokWordFn
  | 21 -> TokWordImport
  | 22 -> TokWordAs
  | 23 -> TokWordFrom
  | 24 -> TokWordAnd
  | 25 -> TokWordOr
  | 26 -> TokWordXor
  | 27 -> TokWordNot
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
  | 43 -> TokPrimBool
  | 44 -> TokLitTrue
  | 45 -> TokLitFalse
  | 46 -> TokIdent
  | 47 -> TokLitIntDec
  | 48 -> TokLitIntHex
  | 49 -> TokLitIntOct
  | 50 -> TokLitIntBin
  | 51 -> TokLitFloat
  | 52 -> TokLitChar
  | 53 -> TokLitStr
  | 54 -> TokLitStrRaw
  | 55 -> TokErr
  | 56 -> TokEof
  | 57 -> TokParenL
  | 58 -> TokParenR
  | 59 -> TokBracketL
  | 60 -> TokBracketR
  | 61 -> TokBraceL
  | 62 -> TokBraceR
  | 63 -> TokDot
  | 64 -> TokComma
  | 65 -> TokSemicolon
  | 66 -> TokColon
  | 67 -> TokEquals
  | 68 -> TokVertLine
  | 69 -> TokAmpersand
  | 70 -> TokTilde
  | 71 -> TokBang
  | 72 -> TokQuestion
  | 73 -> TokCaret
  | 74 -> TokMinus
  | 75 -> TokPlus
  | 76 -> TokStar
  | 77 -> TokSlash
  | 78 -> TokPercent
  | 79 -> TokLess
  | 80 -> TokGreater
  | 81 -> TokCompEq
  | 82 -> TokCompNe
  | 83 -> TokCompLe
  | 84 -> TokCompGe
  | 85 -> TokAddAssign
  | 86 -> TokSubAssign
  | 87 -> TokMulAssign
  | 88 -> TokDivAssign
  | 89 -> TokModAssign
  | 90 -> TokArrow
  | 91 -> TokReturnArrow
  | 92 -> TokFwdCompose
  | 93 -> TokRange
  | 94 -> TokModule
  | 95 -> TokConcat
  | 96 -> TokBitLsl
  | 97 -> TokBitLsr
  | 98 -> TokBitAsl
  | 99 -> TokBitAsr
  | 100 -> TokBitAnd
  | 101 -> TokBitOr
  | 102 -> TokBitXor
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token : int * string -> token = fun (c_typ, c_val) -> {
  token_type = convert_token_type c_typ;
  token_value = if c_val = "" then None else Some c_val;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

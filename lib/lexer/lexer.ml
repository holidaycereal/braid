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
  | TokWordDyn
  | TokWordType
  | TokWordRecord
  | TokWordConst
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
  | 15 -> TokWordDyn
  | 16 -> TokWordType
  | 17 -> TokWordRecord
  | 18 -> TokWordConst
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
  | 84 -> TokArrow
  | 85 -> TokReturnArrow
  | 86 -> TokFwdCompose
  | 87 -> TokPlusEquals
  | 88 -> TokMinusEquals
  | 89 -> TokStarEquals
  | 90 -> TokSlashEquals
  | 91 -> TokCaretEquals
  | 92 -> TokPercentEquals
  | 93 -> TokRange
  | 94 -> TokModule
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token (ttyp, tval) = {
  token_type = convert_token_type ttyp;
  token_value = if tval = "" then None else Some tval;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

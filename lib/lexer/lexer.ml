type token_type =
(* GENERATE BEGIN TYPE {{{ *)
  | TokWordIf
  | TokWordElse
  | TokWordElif
  | TokWordWhile
  | TokWordFor
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
  | 5 -> TokWordIn
  | 6 -> TokWordBreak
  | 7 -> TokWordContinue
  | 8 -> TokWordMatch
  | 9 -> TokWordWith
  | 10 -> TokWordWhen
  | 11 -> TokWordSwitch
  | 12 -> TokWordCase
  | 13 -> TokWordDefault
  | 14 -> TokWordDyn
  | 15 -> TokWordType
  | 16 -> TokWordRecord
  | 17 -> TokWordConst
  | 18 -> TokWordFn
  | 19 -> TokWordInclude
  | 20 -> TokWordImport
  | 21 -> TokWordAs
  | 22 -> TokWordExit
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
  | 35 -> TokPrimUsize
  | 36 -> TokPrimIsize
  | 37 -> TokPrimF32
  | 38 -> TokPrimF64
  | 39 -> TokPrimBool
  | 40 -> TokLitTrue
  | 41 -> TokLitFalse
  | 42 -> TokIdent
  | 43 -> TokLitIntDec
  | 44 -> TokLitIntHex
  | 45 -> TokLitIntOct
  | 46 -> TokLitIntBin
  | 47 -> TokLitFloat
  | 48 -> TokLitChar
  | 49 -> TokLitStr
  | 50 -> TokLitStrRaw
  | 51 -> TokErr
  | 52 -> TokEof
  | 53 -> TokParenL
  | 54 -> TokParenR
  | 55 -> TokBracketL
  | 56 -> TokBracketR
  | 57 -> TokBraceL
  | 58 -> TokBraceR
  | 59 -> TokDot
  | 60 -> TokComma
  | 61 -> TokSemicolon
  | 62 -> TokColon
  | 63 -> TokEquals
  | 64 -> TokVertLine
  | 65 -> TokAmpersand
  | 66 -> TokBang
  | 67 -> TokQuestion
  | 68 -> TokCaret
  | 69 -> TokMinus
  | 70 -> TokPlus
  | 71 -> TokStar
  | 72 -> TokSlash
  | 73 -> TokPercent
  | 74 -> TokLess
  | 75 -> TokGreater
  | 76 -> TokCompEq
  | 77 -> TokCompNe
  | 78 -> TokCompLe
  | 79 -> TokCompGe
  | 80 -> TokArrow
  | 81 -> TokReturnArrow
  | 82 -> TokFwdCompose
  | 83 -> TokPlusEquals
  | 84 -> TokMinusEquals
  | 85 -> TokStarEquals
  | 86 -> TokSlashEquals
  | 87 -> TokCaretEquals
  | 88 -> TokPercentEquals
  | 89 -> TokRange
  | 90 -> TokModule
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr

let convert_token (ttyp, tval) = {
  token_type = convert_token_type ttyp;
  token_value = if tval = "" then None else Some tval;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

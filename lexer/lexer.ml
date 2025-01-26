type token_type =
(* {{{ *)
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
(* }}} *)

type token = {
  tok_typ : token_type;
  tok_val : string option;
}

external c_lex : string -> (int * string) array = "c_lex"

let convert_token_type = function
(* {{{ *)
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
  | 18 -> TokWordInclude
  | 19 -> TokWordImport
  | 20 -> TokWordAs
  | 21 -> TokWordExit
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
  | 34 -> TokPrimUsize
  | 35 -> TokPrimIsize
  | 36 -> TokPrimF32
  | 37 -> TokPrimF64
  | 38 -> TokPrimBool
  | 39 -> TokLitTrue
  | 40 -> TokLitFalse
  | 41 -> TokIdent
  | 42 -> TokLitIntDec
  | 43 -> TokLitIntHex
  | 44 -> TokLitIntOct
  | 45 -> TokLitIntBin
  | 46 -> TokLitFloat
  | 47 -> TokLitChar
  | 48 -> TokLitStr
  | 49 -> TokLitStrRaw
  | 50 -> TokErr
  | 51 -> TokParenL
  | 52 -> TokParenR
  | 53 -> TokBracketL
  | 54 -> TokBracketR
  | 55 -> TokBraceL
  | 56 -> TokBraceR
  | 57 -> TokDot
  | 58 -> TokComma
  | 59 -> TokSemicolon
  | 60 -> TokColon
  | 61 -> TokEquals
  | 62 -> TokVertLine
  | 63 -> TokAmpersand
  | 64 -> TokBang
  | 65 -> TokQuestion
  | 66 -> TokCaret
  | 67 -> TokMinus
  | 68 -> TokPlus
  | 69 -> TokStar
  | 70 -> TokSlash
  | 71 -> TokPercent
  | 72 -> TokLess
  | 73 -> TokGreater
  | 74 -> TokCompEq
  | 75 -> TokCompNe
  | 76 -> TokCompLe
  | 77 -> TokCompGe
  | 78 -> TokArrow
  | 79 -> TokReturnArrow
  | 80 -> TokFwdCompose
  | 81 -> TokPlusEquals
  | 82 -> TokMinusEquals
  | 83 -> TokStarEquals
  | 84 -> TokSlashEquals
  | 85 -> TokCaretEquals
  | 86 -> TokPercentEquals
  | 87 -> TokRange
	| 88 -> TokModule
	| 89 -> TokEof
  | _ -> TokErr
(* }}} *)

let convert_token (ttyp, tval) = {
  tok_typ = convert_token_type ttyp;
  tok_val = if tval = "" then None else Some tval;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

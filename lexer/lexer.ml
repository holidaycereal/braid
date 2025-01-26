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
  | 52 -> TokParenL
  | 53 -> TokParenR
  | 54 -> TokBracketL
  | 55 -> TokBracketR
  | 56 -> TokBraceL
  | 57 -> TokBraceR
  | 58 -> TokDot
  | 59 -> TokComma
  | 60 -> TokSemicolon
  | 61 -> TokColon
  | 62 -> TokEquals
  | 63 -> TokVertLine
  | 64 -> TokAmpersand
  | 65 -> TokBang
  | 66 -> TokQuestion
  | 67 -> TokCaret
  | 68 -> TokMinus
  | 69 -> TokPlus
  | 70 -> TokStar
  | 71 -> TokSlash
  | 72 -> TokPercent
  | 73 -> TokLess
  | 74 -> TokGreater
  | 75 -> TokCompEq
  | 76 -> TokCompNe
  | 77 -> TokCompLe
  | 78 -> TokCompGe
  | 79 -> TokArrow
  | 80 -> TokReturnArrow
  | 81 -> TokFwdCompose
  | 82 -> TokPlusEquals
  | 83 -> TokMinusEquals
  | 84 -> TokStarEquals
  | 85 -> TokSlashEquals
  | 86 -> TokCaretEquals
  | 87 -> TokPercentEquals
  | 88 -> TokRange
	| 89 -> TokModule
	| 90 -> TokEof
  | _ -> TokErr
(* }}} *)

let convert_token (ttyp, tval) = {
  tok_typ = convert_token_type ttyp;
  tok_val = if tval = "" then None else Some tval;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

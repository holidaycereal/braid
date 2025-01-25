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
  | 12 -> TokWordDyn
  | 13 -> TokWordType
  | 14 -> TokWordRecord
  | 15 -> TokWordInclude
  | 16 -> TokWordImport
  | 17 -> TokWordExit
  | 18 -> TokWordAnd
  | 19 -> TokWordOr
  | 20 -> TokWordXor
  | 21 -> TokWordNot
  | 22 -> TokPrimU8
  | 23 -> TokPrimU16
  | 24 -> TokPrimU32
  | 25 -> TokPrimU64
  | 26 -> TokPrimI8
  | 27 -> TokPrimI16
  | 28 -> TokPrimI32
  | 29 -> TokPrimI64
  | 30 -> TokPrimUsize
  | 31 -> TokPrimIsize
  | 32 -> TokPrimF32
  | 33 -> TokPrimF64
  | 34 -> TokPrimBool
  | 35 -> TokLitTrue
  | 36 -> TokLitFalse
  | 37 -> TokIdent
  | 38 -> TokLitIntDec
  | 39 -> TokLitIntHex
  | 40 -> TokLitIntOct
  | 41 -> TokLitIntBin
  | 42 -> TokLitFloat
  | 43 -> TokLitChar
  | 44 -> TokLitStr
  | 45 -> TokLitStrRaw
  | 46 -> TokErr
  | 47 -> TokParenL
  | 48 -> TokParenR
  | 49 -> TokBracketL
  | 50 -> TokBracketR
  | 51 -> TokBraceL
  | 52 -> TokBraceR
  | 53 -> TokDot
  | 54 -> TokComma
  | 55 -> TokSemicolon
  | 56 -> TokColon
  | 57 -> TokEquals
  | 58 -> TokVertLine
  | 59 -> TokAmpersand
  | 60 -> TokBang
  | 61 -> TokQuestion
  | 62 -> TokCaret
  | 63 -> TokMinus
  | 64 -> TokPlus
  | 65 -> TokStar
  | 66 -> TokSlash
  | 67 -> TokPercent
  | 68 -> TokLess
  | 69 -> TokGreater
  | 70 -> TokCompEq
  | 71 -> TokCompNe
  | 72 -> TokCompLe
  | 73 -> TokCompGe
  | 74 -> TokArrow
  | 75 -> TokReturnArrow
  | 76 -> TokFwdCompose
  | 77 -> TokPlusEquals
  | 78 -> TokMinusEquals
  | 79 -> TokStarEquals
  | 80 -> TokSlashEquals
  | 81 -> TokCaretEquals
  | 82 -> TokPercentEquals
  | 83 -> TokRange
	| 84 -> TokModule
	| 85 -> TokEof
  | _ -> TokErr
(* }}} *)

let convert_token (ttyp, tval) = {
  tok_typ = convert_token_type ttyp;
  tok_val = if tval = "" then None else Some tval;
}

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_token c_tokens |> Array.to_list

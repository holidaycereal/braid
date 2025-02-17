type token =
(* GENERATE BEGIN TYPE {{{ *)
  | TokIdent of string
  | TokLitInt of string
  | TokLitFloat of string
  | TokLitChar of string
  | TokLitStr of string
  | TokLitStrRaw of string
  | TokErr of string
  | TokEof
  | TokWordIf
  | TokWordThen
  | TokWordElse
  | TokWordElif
  | TokWordCase
  | TokWordOf
  | TokWordEnd
  | TokWordWhile
  | TokWordFor
  | TokWordIn
  | TokWordDo
  | TokWordDone
  | TokWordBreak
  | TokWordContinue
  | TokWordMatch
  | TokWordWhen
  | TokWordType
  | TokWordRecord
  | TokWordUnion
  | TokWordFn
  | TokWordLet
  | TokWordImport
  | TokWordUse
  | TokWordAnd
  | TokWordOr
  | TokWordXor
  | TokLitTrue
  | TokLitFalse
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
  | TokBang
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
(* GENERATE END TYPE }}} *)

external c_lex : string -> (int * string) array = "c_lex"

let convert_c_token c_token =
  match c_token with
(* GENERATE BEGIN CONVERT {{{ *)
  | (0, s) -> TokIdent s
  | (1, s) -> TokLitInt s
  | (2, s) -> TokLitFloat s
  | (3, s) -> TokLitChar s
  | (4, s) -> TokLitStr s
  | (5, s) -> TokLitStrRaw s
  | (6, s) -> TokErr s
  | (7, _) -> TokEof
  | (8, _) -> TokWordIf
  | (9, _) -> TokWordThen
  | (10, _) -> TokWordElse
  | (11, _) -> TokWordElif
  | (12, _) -> TokWordCase
  | (13, _) -> TokWordOf
  | (14, _) -> TokWordEnd
  | (15, _) -> TokWordWhile
  | (16, _) -> TokWordFor
  | (17, _) -> TokWordIn
  | (18, _) -> TokWordDo
  | (19, _) -> TokWordDone
  | (20, _) -> TokWordBreak
  | (21, _) -> TokWordContinue
  | (22, _) -> TokWordMatch
  | (23, _) -> TokWordWhen
  | (24, _) -> TokWordType
  | (25, _) -> TokWordRecord
  | (26, _) -> TokWordUnion
  | (27, _) -> TokWordFn
  | (28, _) -> TokWordLet
  | (29, _) -> TokWordImport
  | (30, _) -> TokWordUse
  | (31, _) -> TokWordAnd
  | (32, _) -> TokWordOr
  | (33, _) -> TokWordXor
  | (34, _) -> TokLitTrue
  | (35, _) -> TokLitFalse
  | (36, _) -> TokParenL
  | (37, _) -> TokParenR
  | (38, _) -> TokBracketL
  | (39, _) -> TokBracketR
  | (40, _) -> TokBraceL
  | (41, _) -> TokBraceR
  | (42, _) -> TokDot
  | (43, _) -> TokComma
  | (44, _) -> TokSemicolon
  | (45, _) -> TokColon
  | (46, _) -> TokEquals
  | (47, _) -> TokVertLine
  | (48, _) -> TokBang
  | (49, _) -> TokMinus
  | (50, _) -> TokPlus
  | (51, _) -> TokStar
  | (52, _) -> TokSlash
  | (53, _) -> TokPercent
  | (54, _) -> TokLess
  | (55, _) -> TokGreater
  | (56, _) -> TokCompEq
  | (57, _) -> TokCompNe
  | (58, _) -> TokCompLe
  | (59, _) -> TokCompGe
  | (60, _) -> TokArrow
  | (61, _) -> TokReturnArrow
  | (62, _) -> TokFwdCompose
  | (63, _) -> TokRange
  | (64, _) -> TokModule
  | (65, _) -> TokConcat
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr "Unreachable: C enum went out of its range"

let lex input =
  let tokens = c_lex input in
  Array.map convert_c_token tokens

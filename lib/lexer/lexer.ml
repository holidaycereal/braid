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
  | TokWordLet
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
  | TokWordConst
  | TokWordImport
  | TokWordUse
  | TokWordAnd
  | TokWordOr
  | TokWordXor
  | TokWordNot
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
  | TokRangeIncl
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
  | (8, _) -> TokWordLet
  | (9, _) -> TokWordIf
  | (10, _) -> TokWordThen
  | (11, _) -> TokWordElse
  | (12, _) -> TokWordElif
  | (13, _) -> TokWordCase
  | (14, _) -> TokWordOf
  | (15, _) -> TokWordEnd
  | (16, _) -> TokWordWhile
  | (17, _) -> TokWordFor
  | (18, _) -> TokWordIn
  | (19, _) -> TokWordDo
  | (20, _) -> TokWordDone
  | (21, _) -> TokWordBreak
  | (22, _) -> TokWordContinue
  | (23, _) -> TokWordMatch
  | (24, _) -> TokWordWhen
  | (25, _) -> TokWordType
  | (26, _) -> TokWordRecord
  | (27, _) -> TokWordUnion
  | (28, _) -> TokWordFn
  | (29, _) -> TokWordConst
  | (30, _) -> TokWordImport
  | (31, _) -> TokWordUse
  | (32, _) -> TokWordAnd
  | (33, _) -> TokWordOr
  | (34, _) -> TokWordXor
  | (35, _) -> TokWordNot
  | (36, _) -> TokLitTrue
  | (37, _) -> TokLitFalse
  | (38, _) -> TokParenL
  | (39, _) -> TokParenR
  | (40, _) -> TokBracketL
  | (41, _) -> TokBracketR
  | (42, _) -> TokBraceL
  | (43, _) -> TokBraceR
  | (44, _) -> TokDot
  | (45, _) -> TokComma
  | (46, _) -> TokSemicolon
  | (47, _) -> TokColon
  | (48, _) -> TokEquals
  | (49, _) -> TokVertLine
  | (50, _) -> TokBang
  | (51, _) -> TokMinus
  | (52, _) -> TokPlus
  | (53, _) -> TokStar
  | (54, _) -> TokSlash
  | (55, _) -> TokPercent
  | (56, _) -> TokLess
  | (57, _) -> TokGreater
  | (58, _) -> TokCompEq
  | (59, _) -> TokCompNe
  | (60, _) -> TokCompLe
  | (61, _) -> TokCompGe
  | (62, _) -> TokArrow
  | (63, _) -> TokReturnArrow
  | (64, _) -> TokFwdCompose
  | (65, _) -> TokRange
  | (66, _) -> TokRangeIncl
  | (67, _) -> TokModule
  | (68, _) -> TokConcat
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr "Unreachable: C enum went out of its range"

let lex input =
  let c_tokens = c_lex input in
  Array.to_list (Array.map convert_c_token c_tokens)

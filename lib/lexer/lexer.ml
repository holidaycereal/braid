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
  | TokWordAlias
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
  | (29, _) -> TokWordAlias
  | (30, _) -> TokWordImport
  | (31, _) -> TokWordUse
  | (32, _) -> TokWordAnd
  | (33, _) -> TokWordOr
  | (34, _) -> TokWordXor
  | (35, _) -> TokLitTrue
  | (36, _) -> TokLitFalse
  | (37, _) -> TokParenL
  | (38, _) -> TokParenR
  | (39, _) -> TokBracketL
  | (40, _) -> TokBracketR
  | (41, _) -> TokBraceL
  | (42, _) -> TokBraceR
  | (43, _) -> TokDot
  | (44, _) -> TokComma
  | (45, _) -> TokSemicolon
  | (46, _) -> TokColon
  | (47, _) -> TokEquals
  | (48, _) -> TokVertLine
  | (49, _) -> TokBang
  | (50, _) -> TokMinus
  | (51, _) -> TokPlus
  | (52, _) -> TokStar
  | (53, _) -> TokSlash
  | (54, _) -> TokPercent
  | (55, _) -> TokLess
  | (56, _) -> TokGreater
  | (57, _) -> TokCompEq
  | (58, _) -> TokCompNe
  | (59, _) -> TokCompLe
  | (60, _) -> TokCompGe
  | (61, _) -> TokArrow
  | (62, _) -> TokReturnArrow
  | (63, _) -> TokFwdCompose
  | (64, _) -> TokRange
  | (65, _) -> TokModule
  | (66, _) -> TokConcat
(* GENERATE END CONVERT }}} *)
  | _ -> TokErr "Unreachable: C enum went out of its range"

let lex input =
  let c_tokens = c_lex input in
  Array.map convert_c_token c_tokens

open Lexer
open Printf

let print_token token =
  let token_string = match token with
(* GENERATE BEGIN PRINT {{{ *)
  | TokIdent s -> "TokIdent " ^ s
  | TokLitInt s -> "TokLitInt " ^ s
  | TokLitFloat s -> "TokLitFloat " ^ s
  | TokLitChar s -> "TokLitChar " ^ s
  | TokLitStr s -> "TokLitStr " ^ s
  | TokLitStrRaw s -> "TokLitStrRaw " ^ s
  | TokErr s -> "TokErr " ^ s
  | TokEof -> "TokEof"
  | TokWordLet -> "TokWordLet"
  | TokWordIf -> "TokWordIf"
  | TokWordThen -> "TokWordThen"
  | TokWordElse -> "TokWordElse"
  | TokWordElif -> "TokWordElif"
  | TokWordCase -> "TokWordCase"
  | TokWordOf -> "TokWordOf"
  | TokWordEnd -> "TokWordEnd"
  | TokWordWhile -> "TokWordWhile"
  | TokWordFor -> "TokWordFor"
  | TokWordIn -> "TokWordIn"
  | TokWordDo -> "TokWordDo"
  | TokWordDone -> "TokWordDone"
  | TokWordBreak -> "TokWordBreak"
  | TokWordContinue -> "TokWordContinue"
  | TokWordMatch -> "TokWordMatch"
  | TokWordWhen -> "TokWordWhen"
  | TokWordType -> "TokWordType"
  | TokWordRecord -> "TokWordRecord"
  | TokWordUnion -> "TokWordUnion"
  | TokWordFn -> "TokWordFn"
  | TokWordAlias -> "TokWordAlias"
  | TokWordImport -> "TokWordImport"
  | TokWordUse -> "TokWordUse"
  | TokWordAnd -> "TokWordAnd"
  | TokWordOr -> "TokWordOr"
  | TokWordXor -> "TokWordXor"
  | TokWordNot -> "TokWordNot"
  | TokLitTrue -> "TokLitTrue"
  | TokLitFalse -> "TokLitFalse"
  | TokParenL -> "TokParenL"
  | TokParenR -> "TokParenR"
  | TokBracketL -> "TokBracketL"
  | TokBracketR -> "TokBracketR"
  | TokBraceL -> "TokBraceL"
  | TokBraceR -> "TokBraceR"
  | TokDot -> "TokDot"
  | TokComma -> "TokComma"
  | TokSemicolon -> "TokSemicolon"
  | TokColon -> "TokColon"
  | TokEquals -> "TokEquals"
  | TokVertLine -> "TokVertLine"
  | TokBang -> "TokBang"
  | TokMinus -> "TokMinus"
  | TokPlus -> "TokPlus"
  | TokStar -> "TokStar"
  | TokSlash -> "TokSlash"
  | TokPercent -> "TokPercent"
  | TokLess -> "TokLess"
  | TokGreater -> "TokGreater"
  | TokCompEq -> "TokCompEq"
  | TokCompNe -> "TokCompNe"
  | TokCompLe -> "TokCompLe"
  | TokCompGe -> "TokCompGe"
  | TokArrow -> "TokArrow"
  | TokReturnArrow -> "TokReturnArrow"
  | TokFwdCompose -> "TokFwdCompose"
  | TokRange -> "TokRange"
  | TokRangeIncl -> "TokRangeIncl"
  | TokModule -> "TokModule"
  | TokConcat -> "TokConcat"
(* GENERATE END PRINT }}} *)
  in
  printf "%s\n" token_string

let () =
  if Array.length Sys.argv <> 2 then (
    eprintf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let input =
    try
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    with
    | Sys_error msg ->
        eprintf "Error: %s\n" msg;
        exit 1
  in
  let tokens = lex input in
  List.iter print_token tokens

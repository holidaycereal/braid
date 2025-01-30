open Lexer

let print_token token =
  let typ_str = match token.token_type with
(* GENERATE BEGIN PRINT {{{ *)
  | TokWordIf -> "TokWordIf"
  | TokWordElse -> "TokWordElse"
  | TokWordElif -> "TokWordElif"
  | TokWordWhile -> "TokWordWhile"
  | TokWordFor -> "TokWordFor"
  | TokWordDo -> "TokWordDo"
  | TokWordIn -> "TokWordIn"
  | TokWordBreak -> "TokWordBreak"
  | TokWordContinue -> "TokWordContinue"
  | TokWordMatch -> "TokWordMatch"
  | TokWordWith -> "TokWordWith"
  | TokWordWhen -> "TokWordWhen"
  | TokWordSwitch -> "TokWordSwitch"
  | TokWordCase -> "TokWordCase"
  | TokWordDefault -> "TokWordDefault"
  | TokWordType -> "TokWordType"
  | TokWordRecord -> "TokWordRecord"
  | TokWordConst -> "TokWordConst"
  | TokWordAlias -> "TokWordAlias"
  | TokWordFn -> "TokWordFn"
  | TokWordInclude -> "TokWordInclude"
  | TokWordImport -> "TokWordImport"
  | TokWordAs -> "TokWordAs"
  | TokWordExit -> "TokWordExit"
  | TokWordAnd -> "TokWordAnd"
  | TokWordOr -> "TokWordOr"
  | TokWordXor -> "TokWordXor"
  | TokPrimByte -> "TokPrimByte"
  | TokPrimUint -> "TokPrimUint"
  | TokPrimU8 -> "TokPrimU8"
  | TokPrimU16 -> "TokPrimU16"
  | TokPrimU32 -> "TokPrimU32"
  | TokPrimU64 -> "TokPrimU64"
  | TokPrimInt -> "TokPrimInt"
  | TokPrimI8 -> "TokPrimI8"
  | TokPrimI16 -> "TokPrimI16"
  | TokPrimI32 -> "TokPrimI32"
  | TokPrimI64 -> "TokPrimI64"
  | TokPrimUsize -> "TokPrimUsize"
  | TokPrimIsize -> "TokPrimIsize"
  | TokPrimF32 -> "TokPrimF32"
  | TokPrimF64 -> "TokPrimF64"
  | TokPrimFloat -> "TokPrimFloat"
  | TokPrimDyn -> "TokPrimDyn"
  | TokPrimBool -> "TokPrimBool"
  | TokLitTrue -> "TokLitTrue"
  | TokLitFalse -> "TokLitFalse"
  | TokIdent -> "TokIdent"
  | TokLitIntDec -> "TokLitIntDec"
  | TokLitIntHex -> "TokLitIntHex"
  | TokLitIntOct -> "TokLitIntOct"
  | TokLitIntBin -> "TokLitIntBin"
  | TokLitFloat -> "TokLitFloat"
  | TokLitChar -> "TokLitChar"
  | TokLitStr -> "TokLitStr"
  | TokLitStrRaw -> "TokLitStrRaw"
  | TokErr -> "TokErr"
  | TokEof -> "TokEof"
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
  | TokAmpersand -> "TokAmpersand"
  | TokHash -> "TokHash"
  | TokTilde -> "TokTilde"
  | TokBang -> "TokBang"
  | TokQuestion -> "TokQuestion"
  | TokCaret -> "TokCaret"
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
  | TokModule -> "TokModule"
  | TokConcat -> "TokConcat"
  | TokBitLsl -> "TokBitLsl"
  | TokBitLsr -> "TokBitLsr"
  | TokBitAsl -> "TokBitAsl"
  | TokBitAsr -> "TokBitAsr"
(* GENERATE END PRINT }}} *)
  in
  let val_str = match token.token_value with
  | None -> ""
  | Some v -> v
  in
  Printf.printf "%s \027[34m%s\027[0m\n" typ_str val_str

let () =
  if Array.length Sys.argv <> 2 then (
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0);
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
        Printf.eprintf "Error: %s\n" msg;
        exit 1
  in

  let tokens = lex input in
  List.iter print_token tokens

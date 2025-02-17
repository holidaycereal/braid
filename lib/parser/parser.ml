open Grammar
open Lexer
open Stdint

let (>>=) = Result.bind

type state =
  { stream : token array
  ; pos : int
  }

type parser_error =
  | LexerError of string
  | MissingCloseDelim
  | UnterminatedExpr
  | UnterminatedStmt
  | UnterminatedLiteral
  | ExpectedTopLevel
  | ExpectedLiteralOrIdentifier

let peek state = state.stream.(state.pos)
let move state n = { state with pos = state.pos + n }
let look state n = peek (move state n)
let next state = move state 1

let op_prec = function
  | TokModule -> 10
  | TokDot -> 9
  | TokStar | TokSlash | TokPercent -> 8
  | TokPlus | TokMinus -> 7
  | TokConcat | TokRange -> 6
  | TokLess | TokGreater | TokCompEq | TokCompNe | TokCompLe | TokCompGe -> 5
  | TokWordAnd -> 3
  | TokWordXor -> 2
  | TokWordOr -> 1
  | _ -> 0

(* Helper to get tokens until the first instance of delim *)
let tokens_til delim state =
  let rec aux pos accum =
    if pos >= Array.length state.stream then Error MissingCloseDelim
    else if state.stream.(pos) = delim then Ok (List.rev accum)
    else aux (pos + 1) (state.stream.(pos) :: accum)
  in
  aux state.pos []

(* Helpers to get tokens in a pair of parens, brackets or braces *)
let tokens_in opener closer state =
  let rec aux pos depth accum =
    if pos >= Array.length state.stream then Error MissingCloseDelim
    else
      let tok = state.stream.(pos) in
      if tok = opener then aux (pos + 1) (depth + 1) (tok :: accum)
      else if tok = closer then
        if depth = 0 then Ok (List.rev accum)
        else aux (pos + 1) (depth - 1) (tok :: accum)
      else aux (pos + 1) depth (tok :: accum)
  in
  aux state.pos 0 []

let tokens_in_parens state = tokens_in TokParenL TokParenR state
let tokens_in_brackets state = tokens_in TokBracketL TokBracketR state
let tokens_in_braces state = tokens_in TokBraceL TokBraceR state

(* Expression parser *)
let rec parse_expr tokens =
  if List.length tokens < 1 then Ok Unit (* Empty parens *)
  else if List.length tokens = 1 then (* Single token expression *)
    match List.hd tokens with
    | TokIdent name -> Ok (Reference name)
    | TokLitInt lit -> parse_int lit >>= fun n -> Ok (IntLiteral n)
    | TokLitFloat lit -> parse_float lit >>= fun n -> Ok (FloatLiteral n)
    | TokLitChar lit -> parse_char lit >>= fun c -> Ok (IntLiteral c)
    | TokLitStr lit -> parse_str lit >>= fun s -> Ok (ListLiteral s)
    | TokLitStrRaw lit -> parse_raw_str lit >>= fun s -> Ok (ListLiteral s)
    | TokLitTrue -> Ok (BoolLiteral true)
    | TokLitFalse -> Ok (BoolLiteral false)
    | _ -> Error ExpectedLiteralOrIdentifier
  else
    parse_primary tokens >>= fun (pos, expr) ->
    if pos + 1 = List.length tokens then (* Whole expression parsed *)
      Ok expr
    else (* Binary expression *)

and parse_primary tokens =
  let pos = 0 in
  let aux tokens pos =
    match List.hd tokens with
    | TokIdent name ->
        match List.nth tokens 1 with
        | TokParenL -> (* Function application *)
    | TokParenL
    | TokBraceL (* record literal *)
    | TokWordMatch
    | TokWordWhen

(* Top-level statement parser *)
let parse_top_level state =
  match peek state with
  | TokWordImport -> parse_import (next state)
  | TokWordLet -> parse_expr_def (next state)
  | TokWordFn ->
      parse_ident (next state) >>= fun (name, state) ->
      parse_fn_params state >>= fun (params, state) ->
      parse_type_sig state >>= fun (type_sig, state) ->
      parse_fn_body state >>= fun (body, state) ->
      Ok (FnDef { name; params; type_sig; body }, state)
  | TokWordType ->
      parse_ident (next state) >>= fun (name, state) ->
      parse_type_params state >>= fun (type_params, state) ->
      parse_typedef_expr state >>= fun (value, state) ->
      Ok (TypeDef { name; type_params; value }, state)
  | TokWordRecord ->
      parse_ident (next state) >>= fun (name, state) ->
      parse_type_params state >>= fun (type_params, state) ->
      parse_record_body state >>= fun (body, state) ->
      Ok (RecordDef { name; type_params; body }, state)
  | TokWordUnion ->
      parse_ident (next state) >>= fun (name, state) ->
      parse_type_params state >>= fun (type_params, state) ->
      parse_union_body state >>= fun (body, state) ->
      Ok (UnionDef { name; type_params; body }, state)
  | _ -> Error ExpectedTopLevel

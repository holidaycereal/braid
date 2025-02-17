open Grammar
open Lexer
open Stdint

let (>>=) = Result.bind

type state =
  { stream : token list
  ; pos : int
  }

type parser_error =
  (* Propagate lexer error *)
  | LexerError of string
  (* Unclosed statements or expressions *)
  | MissingCloseDelim
  | UnterminatedExpr
  | UnterminatedStmt
  | UnterminatedLiteral
  (* Expected tokens vs actual token *)
  | Expected of token list * token
  | ExpectedIdent of token
  | ExpectedLiteral of token
  | ExpectedTopLevel of token

let peek state = List.nth state.stream state.pos
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
    if pos >= List.length state.stream then Error MissingCloseDelim
    else if List.nth state.stream pos = delim then Ok (List.rev accum)
    else aux (pos + 1) (List.nth state.stream pos :: accum)
  in
  aux state.pos []

(* Helpers to get tokens in a pair of parens, brackets or braces *)
let tokens_in opener closer state =
  let rec aux pos depth accum =
    if pos >= List.length state.stream then Error MissingCloseDelim
    else
      let tok = List.nth state.stream pos in
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

(* expression parser *)
(* {{{ *)
(*let parse_primary tokens =*)
(*  if List.length tokens < 1 then Ok Unit (* Empty parens *)*)
(*  else if List.length tokens = 1 then (* Single token expression *)*)
(*    match List.hd tokens with*)
(*    | TokIdent name -> Ok (1, Reference name)*)
(*    | TokLitInt lit -> parse_int lit >>= fun n -> Ok (1, IntLiteral n)*)
(*    | TokLitFloat lit -> parse_float lit >>= fun n -> Ok (1, FloatLiteral n)*)
(*    | TokLitChar lit -> parse_char lit >>= fun c -> Ok (1, IntLiteral c)*)
(*    | TokLitStr lit -> parse_str lit >>= fun s -> Ok (1, ListLiteral s)*)
(*    | TokLitStrRaw lit -> parse_raw_str lit >>= fun s -> Ok (1, ListLiteral s)*)
(*    | TokLitTrue -> Ok (1, BoolLiteral true)*)
(*    | TokLitFalse -> Ok (1, BoolLiteral false)*)
(*    | _ -> Error ExpectedLiteralOrIdentifier*)
(*  else*)
(* }}} *)

let parse_ident state =
  match peek state with
  | TokIdent name -> Ok (name, next state)
  | tok -> Error (ExpectedIdent tok)

let parse_ident_list state delim =
  let rec aux state accum =
    match peek state with
    | tok when tok = delim -> Ok (List.rev accum, next state)
    | TokIdent item ->
        let state = next state in
        begin match peek state with
        | TokComma -> aux (next state) (item :: accum)
        | tok -> Error (Expected ([TokComma], tok))
        end
    | tok -> Error (ExpectedIdent tok)
  in
  aux state []

let parse_import state =
  parse_ident state >>= fun (name, state) ->
  begin match peek state with
  | TokSemicolon -> Ok (Import (name, []), next state)
  | TokModule ->
      parse_ident_list (next state) TokSemicolon >>= fun (items, state) ->
      Ok (Import (name, items), state)
  | tok -> Error (Expected ([TokSemicolon; TokModule], tok))
  end

let parse_alias_def state =
  parse_ident state >>= fun (name, state) ->
  match peek state with
  | TokEquals ->
      parse_expr (next state) TokSemicolon >>= fun (value, state) ->
      Ok (AliasDef { name; params = []; value }, state)
  | TokParenL ->
      parse_ident_list (next state) TokParenR >>= fun (params, state) ->
      begin match peek state with
      | TokEquals ->
          parse_expr (next state) TokSemicolon >>= fun (value, state) ->
          Ok (AliasDef { name; params; value }, state)
      | tok -> Error (Expected ([TokEquals], tok))
      end
  | tok -> Error (Expected ([TokEquals; TokParenL], tok))

let parse_fn_def state =
  parse_ident state >>= fun (name, state) ->
  parse_fn_params state >>= fun (params, state) ->
  parse_fn_type state >>= fun (type_sig, state) ->
  parse_fn_body state >>= fun (body, state) ->
  Ok (FnDef { name; params; type_sig; body }, state)

let parse_type_def state =
  parse_ident state >>= fun (name, state) ->
  parse_type_params state >>= fun (params, state) ->
  parse_type_expr state >>= fun (value, state) ->
  Ok (TypeDef { name; params; value }, state)

let parse_union_def state =
  parse_ident state >>= fun (name, state) ->
  parse_type_params state >>= fun (params, state) ->
  parse_union_body state >>= fun (body, state) ->
  Ok (UnionDef { name; params; body }, state)

let parse_record_def state =
  parse_ident state >>= fun (name, state) ->
  parse_type_params state >>= fun (params, state) ->
  parse_record_body state >>= fun (body, state) ->
  Ok (RecordDef { name; params; body }, state)

(* Top-level statement parser *)
let parse_top_level state =
  match peek state with
  | TokWordImport -> parse_import (next state)
  | TokWordAlias -> parse_alias_def (next state)
  | TokWordFn -> parse_fn_def (next state)
  | TokWordType -> parse_type_def (next state)
  | TokWordUnion -> parse_union_def (next state)
  | TokWordRecord -> parse_record_def (next state)
  | tok -> Error (ExpectedTopLevel tok)

(* Main parser function *)
let parse state =
  let rec loop state accum =
    match peek state with
    | TokEof -> Ok (List.rev accum)
    | _ ->
        parse_top_level state >>= fun (def, state) ->
        loop state (def :: accum)
  in
  loop state []

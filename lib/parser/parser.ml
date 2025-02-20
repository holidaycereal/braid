open Grammar
open Lexer
open Stdint

let (>>=) = Result.bind

type state =
  { stream : token list
  ; pos : int
  }

let peek state = List.nth state.stream state.pos
let move state n = { state with pos = state.pos + n }
let look state n = peek (move state n)
let next state = move state 1

type parser_error =
  | Unreachable
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
  (* Expected node vs actual node *)
  | ExpectedFnType of node
  (* Invalid constructs *)
  | InvalidVariant
  | InvalidUnionBody
  | InvalidSelfConstructor
  | InvalidField
  | InvalidRecordBody

let op_prec = function
  | TokModule -> 10
  | TokDot -> 9
  | TokStar | TokSlash | TokPercent -> 8
  | TokPlus | TokMinus -> 7
  | TokConcat | TokRange | TokRangeIncl -> 6
  | TokLess | TokGreater | TokCompLe | TokCompGe -> 5
  | TokCompEq | TokCompNe -> 4
  | TokWordAnd -> 3
  | TokWordXor -> 2
  | TokWordOr -> 1
  | _ -> 0

(* Helper to get tokens until the first instance of any of delims *)
let tokens_til delims state =
  let rec aux state accum =
    let tok = peek state in
    if tok = TokEof then Error MissingCloseDelim
    else if List.mem tok delims then Ok (List.rev (TokEof :: accum))
    else aux (next state) (tok :: accum)
  in
  aux state []

(* Helpers to get tokens in a pair of parens, brackets or braces *)
let tokens_in opener closer state =
  let rec aux state depth accum =
    if peek state = TokEof then Error MissingCloseDelim
    else
      let tok = peek state in
      if tok = opener then aux (next state) (depth + 1) (tok :: accum)
      else if tok = closer then
        if depth = 0 then Ok (List.rev (TokEof :: accum))
        else aux (next state) (depth - 1) (tok :: accum)
      else aux (next state) depth (tok :: accum)
  in
  aux state 0 []

let tokens_in_parens state = tokens_in TokParenL TokParenR state
let tokens_in_brackets state = tokens_in TokBracketL TokBracketR state
let tokens_in_braces state = tokens_in TokBraceL TokBraceR state

(* Similar to tokens_til, but also take into account parens, brackets and braces
   so this: `x(y, z),` won't stop until the last comma *)
let tokens_til_matching delims state =
  let rec aux state accum =
    match peek state with
    | TokParenL ->
        tokens_in_parens (next state) >>= fun tokens ->
        aux (move state (List.length tokens)) ((List.rev tokens) @ accum)
    | TokBracketL ->
        tokens_in_brackets (next state) >>= fun tokens ->
        aux (move state (List.length tokens)) ((List.rev tokens) @ accum)
    | TokBraceL ->
        tokens_in_braces (next state) >>= fun tokens ->
        aux (move state (List.length tokens)) ((List.rev tokens) @ accum)
    | tok ->
        if List.mem tok delims then Ok (List.rev (TokEof :: accum))
        else aux (next state) (tok :: accum)
  in
  aux state []

(* Helper to consume an identifier *)
let parse_ident state =
  match peek state with
  | TokIdent name -> Ok (name, next state)
  | tok -> Error (ExpectedIdent tok)

(* Parse a list of comma-separated identifiers *)
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

(* Parse module imports *)
let parse_import state =
  parse_ident state >>= fun (name, state) ->
  begin match peek state with
  | TokSemicolon -> Ok (Import (name, []), next state)
  | TokModule ->
      parse_ident_list (next state) TokSemicolon >>= fun (items, state) ->
      Ok (Import (name, items), state)
  | tok -> Error (Expected ([TokSemicolon; TokModule], tok))
  end

(* Top-level expression definitions *)
let parse_alias_def state =
  parse_ident state >>= fun (name, state) ->
  match peek state with
  | TokEquals ->
      tokens_til [TokSemicolon] (next state) >>= fun tokens ->
      parse_expr tokens >>= fun expr ->
      Ok (AliasDef (name, expr), move state (List.length tokens))
  | tok -> Error (Expected ([TokEquals], tok))

(* Function definitions *)
let rec parse_fn_def state =
  parse_ident state >>= fun (name, state) ->
  parse_fn_params state >>= fun (params, state) ->
  parse_fn_type state >>= fun (type_sig, state) ->
  parse_fn_body state >>= fun (body, state) ->
  Ok (FnDef { name; params; type_sig; body }, state)

and parse_fn_params state =
  let rec aux state accum =
    match peek state with
    | TokColon | TokBraceL | TokArrow | TokReturnArrow ->
        Ok (List.rev accum, state)
    | TokParenL ->
        parse_ident_list (next state) TokParenR >>= fun (names, state) ->
        aux state (names :: accum)
    | tok ->
        Error (Expected ([
          TokColon; TokBraceL; TokArrow; TokReturnArrow; TokParenL
        ], tok))
  in
  aux state []

and parse_fn_type state =
  tokens_til [TokBraceL; TokReturnArrow] state >>= fun tokens ->
  parse_type_expr tokens >>= fun expr ->
  match expr with
  | FnType _ -> Ok (expr, move state (List.length tokens))
  | _ -> Error (ExpectedFnType expr)

and parse_fn_body state =
  match peek state with
  | TokReturnArrow ->
      tokens_til [TokSemicolon] (next state) >>= fun tokens ->
      parse_expr tokens >>= fun expr ->
      Ok ([ReturnStmt expr], move state (List.length tokens + 1))
  | TokBraceL ->
      tokens_in_braces (next state) >>= fun tokens ->
      parse_stmt_list tokens >>= fun body ->
      Ok (body, move state (List.length tokens + 2))
  | _ ->
      Error Unreachable

(* Helper for parsing type parameters *)
let parse_type_params state =
  match peek state with
  | TokBracketL ->
      parse_ident_list (next state) TokBracketR >>= fun (params, state) ->
      Ok (params, state)
  | _ -> Ok ([], next state)

(* Type alias definitions *)
let parse_type_def state =
  parse_ident state >>= fun (name, state) ->
  parse_type_params state >>= fun (params, state) ->
  tokens_til [TokSemicolon] state >>= fun tokens ->
  parse_type_expr tokens >>= fun value ->
  Ok (TypeDef { name; params; value }, (move state (List.length tokens)))

(* Union/sum type definitions *)
let rec parse_union_def state =
  parse_ident state >>= fun (name, state) ->
  parse_type_params state >>= fun (params, state) ->
  parse_union_body state >>= fun (body, state) ->
  Ok (UnionDef { name; params; body }, state)

and parse_union_body state =
  match peek state with
  | TokBraceL ->
      tokens_in_braces (next state) >>= fun tokens ->
      parse_variant_list tokens >>= fun body ->
      Ok (body, move state (List.length tokens))
  | tok -> Error (Expected ([TokBraceL], tok))

and parse_variant_list tokens =
  let rec aux state accum =
    match peek state with
    | TokEof -> Ok (List.rev accum) (* Finished *)
    | TokComma -> aux (next state) accum (* Go to next variant *)
    | TokIdent name ->
        let state = next state in
        begin match peek state with
        (* Plain value constructor *)
        | TokEof | TokComma -> aux (next state) (PureVariant name :: accum)
        (* Tuple type constructor *)
        | TokParenL ->
            tokens_in_parens (next state) >>= fun tokens ->
            parse_type_list tokens >>= fun types ->
            aux (move state (List.length tokens))
                (ConstructorVariant (name, TupleType types) :: accum)
        (* Record constructor *)
        | TokBraceL ->
            tokens_in_braces (next state) >>= fun tokens ->
            parse_field_list tokens >>= fun fields ->
            aux (move state (List.length tokens))
                (RecordVariant (name, fields) :: accum)
        | _ -> Error InvalidVariant
        end
    | TokParenL -> (* Self constructor *)
        let state = next state in
        begin match (peek state, look state 1) with
        | (TokIdent name, TokParenR) ->
            aux (move state 2) (SelfConstructorVariant name :: accum)
        | _ -> Error InvalidSelfConstructor
        end
    | _ -> Error InvalidUnionBody
  in
  aux { stream = tokens; pos = 0 } []

(* Record/named product type definitions.
   Record and union definitions are circularly dependent. *)
and parse_record_def state =
  parse_ident state >>= fun (name, state) ->
  parse_type_params state >>= fun (params, state) ->
  parse_record_body state >>= fun (body, state) ->
  Ok (RecordDef { name; params; body }, state)

and parse_record_body state =
  match peek state with
  | TokBraceL ->
      tokens_in_braces (next state) >>= fun tokens ->
      parse_field_list tokens >>= fun body ->
      Ok (body, move state (List.length tokens))
  | tok -> Error (Expected ([TokBraceL], tok))

and parse_field_list tokens =
  let rec aux state accum =
    match peek state with
    | TokEof -> Ok (List.rev accum)
    | TokComma -> aux (next state) accum
    | TokIdent name ->
        let state = next state in
        begin match peek state with
        (* Normal field: identifier and a type *)
        | TokColon ->
            tokens_til_matching [TokComma; TokEof] (next state) >>= fun tokens ->
            parse_type_expr tokens >>= fun expr ->
            aux (move state (List.length tokens))
                (DeclField (name, expr) :: accum)
        (* Union field *)
        | TokBraceL ->
            tokens_in_braces (next state) >>= fun tokens ->
            parse_variant_list tokens >>= fun variants ->
            aux (move state (List.length tokens))
                (UnionField (name, variants) :: accum)
        | _ -> Error InvalidField
        end
    | _ -> Error InvalidRecordBody
  in
  aux { stream = tokens; pos = 0 } []

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

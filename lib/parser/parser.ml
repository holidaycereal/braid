open Stdint
open List
open Lexer
open Types

let (>>=) = Result.bind

let can_compound_assign = function
  | TokMinus | TokPlus | TokStar | TokSlash | TokPercent | TokConcat -> true
  | _ -> false

let peek state = nth state.stream state.pos
let move state n = { state with pos = state.pos + n }
let look state n = peek (move state n)
let next state = move state 1
let prev state = move state (-1)

let goto_next delims state =
  let rec aux state =
    let tok = peek state in
    if tok = TokEof then Error MissingCloseDelim
    else if mem tok delims then Ok state 
    else aux (next state)
  in
  aux state

let goto_last delims state =
  let rec aux state =
    if state.pos = 0 then Error MissingCloseDelim
    else if mem (peek state) delims then Ok state
    else aux (prev state)
  in
  aux state

let distance_last delims state =
  goto_last delims state >>= fun last_state ->
  Ok (state.pos - last_state.pos)

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
    else if mem tok delims then Ok (rev (TokEof :: accum))
    else aux (next state) (tok :: accum)
  in
  aux state []

(* Tokens between two states *)
let tokens_between state1 state2 =
  let rec aux state accum =
    if state.pos = state2.pos then rev (TokEof :: accum)
    else aux (next state) (peek state :: accum)
  in
  aux state1 []

(* Helpers to get tokens in a pair of parens, brackets or braces *)
let tokens_in opener closer state =
  let rec aux state depth accum =
    if peek state = TokEof then Error MissingCloseDelim
    else
      let tok = peek state in
      if tok = opener then aux (next state) (depth + 1) (tok :: accum)
      else if tok = closer then
        if depth = 0 then Ok (rev (TokEof :: accum))
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
        aux (move state (length tokens)) ((rev tokens) @ accum)
    | TokBracketL ->
        tokens_in_brackets (next state) >>= fun tokens ->
        aux (move state (length tokens)) ((rev tokens) @ accum)
    | TokBraceL ->
        tokens_in_braces (next state) >>= fun tokens ->
        aux (move state (length tokens)) ((rev tokens) @ accum)
    | tok ->
        if mem tok delims then Ok (rev (TokEof :: accum))
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
    | tok when tok = delim -> Ok (rev accum, next state)
    | TokIdent item ->
        let state = next state in
        begin match peek state with
        | TokComma -> aux (next state) (item :: accum)
        | tok -> Error (Expected ([TokComma], tok))
        end
    | tok -> Error (ExpectedIdent tok)
  in
  aux state []

(* Parse a statement *)
let rec parse_stmt state =
  match peek state with
  (* Declaration *)
  | TokWordLet -> parse_declaration state

  (* Control flow *)
  | TokReturnArrow ->
      let state = next state in
      tokens_til [TokSemicolon] state >>= fun tokens ->
      parse_expr tokens >>= fun expr ->
      Ok (ReturnStmt expr, move state (length tokens))
  | TokWordContinue ->
      begin match look state 1 with
      | TokSemicolon -> Ok (Continue, move state 2)
      | tok -> Error (Expected ([TokSemicolon], tok))
      end
  | TokWordBreak ->
      begin match look state 1 with
      | TokSemicolon -> Ok (Break, move state 2)
      | tok -> Error (Expected ([TokSemicolon], tok))
      end

  (* Imperative blocks *)
  | TokWordIf -> parse_if_stmt (next state) []
  | TokWordCase -> parse_case_stmt (next state)
  | TokWordWhile -> parse_while_loop (next state)
  | TokWordFor -> parse_for_loop (next state)

  (* Default case: try to parse an assignment or function call *)
  | _ ->
      tokens_til [TokSemicolon] state >>= fun tokens ->
      if mem TokEquals tokens then (* Assignment *)
        goto_next [TokEquals] state >>= fun equals_pos ->
        let operator = look equals_pos (-1) in
        begin match operator with
        | TokMinus | TokPlus | TokStar | TokSlash | TokPercent | TokConcat ->
            (* Compound assignment *)
            tokens_til [TokEquals] state >>= fun tmp ->
            let lhs_toks = (drop 1 tmp) @ [TokEof] in
            tokens_til [TokSemicolon] (next equals_pos) >>= fun rhs_toks ->
            parse_expr lhs_toks >>= fun lhs ->
            parse_expr rhs_toks >>= fun rhs ->
            goto_next [TokSemicolon] state >>= fun state ->
            Ok (Assignment (lhs, BinOp (operator, lhs, rhs)), next state)
        | TokVertLine ->
            (* Compound infix function application assignment *)
            goto_last [TokVertLine] equals_pos >>= fun tmp ->
            let fn_app_start = next tmp in
            tokens_til [TokVertLine] fn_app_start >>= fun app_toks ->
            let tmp = tokens_between state equals_pos in
            let lhs_toks = take (length tmp - (length app_toks + 1)) tmp in
            tokens_til [TokSemicolon] (next equals_pos) >>= fun rhs_toks ->
            parse_expr app_toks >>= fun app ->
            parse_expr lhs_toks >>= fun lhs ->
            parse_expr rhs_toks >>= fun rhs ->
            goto_next [TokSemicolon] state >>= fun state ->
            let state = next state in
            begin match app with
            | FnApp (name, args) ->
                Ok (Assignment (lhs, FnApp (name, args @ lhs :: rhs)), state)
            | Reference name ->
                Ok (Assignment (lhs, FnApp (name, lhs :: rhs)), state)
            | _ -> Error InvalidInfixFn
            end
        | _ ->
            (* Normal assignment *)
            let lhs_tokens = tokens_between state equals_pos in
            tokens_til [TokSemicolon] (next equals_pos) >>= fun rhs_tokens ->
            parse_expr lhs_tokens >>= fun lhs ->
            parse_expr rhs_tokens >>= fun rhs ->
            goto_next [TokSemicolon] state >>= fun state ->
            let state = next state in
            Ok (Assignment (lhs, rhs), state)
        end
      else
        parse_expr tokens >>= fun expr ->
        begin match expr with
        | FnApp (name, args) ->
            Ok (FnCall (FnApp (name, args)), move state (length tokens))
        | _ -> Error (ExpectedFnCall expr)
        end

and parse_while_loop state =
  tokens_til [TokWordDo] state >>= fun tokens ->
  parse_expr tokens >>= fun cond ->
  let state = move state (length tokens) in
  tokens_in TokWordDo TokWordDone state >>= fun tokens ->
  parse_stmt_list tokens >>= fun body ->
  Ok (WhileLoop { cond; body }, move state (length tokens))

and parse_for_loop state =
  tokens_til [TokWordIn] state >>= fun tokens ->
  parse_pattern tokens >>= fun capture ->
  let state = move state (length tokens) in
  tokens_til [TokWordDo] state >>= fun tokens ->
  parse_expr tokens >>= fun iterator ->
  let state = move state (length tokens) in
  tokens_in TokWordDo TokWordDone state >>= fun tokens ->
  parse_stmt_list tokens >>= fun body ->
  Ok (ForLoop { capture; iterator; body }, move state (length tokens))

and parse_declaration state =
  let expect_equals state =
    match peek state with
    | TokEquals ->
        let state = next state in
        tokens_til [TokSemicolon] state >>= fun tokens ->
        parse_expr tokens >>= fun expr ->
        Ok (expr, move state (length tokens))
    | tok -> Error (Expected ([TokEquals], tok))
  in
  let state = next state in
  parse_ident state >>= fun (name, state) ->
  begin match peek state with
  | TokColon ->
      let state = next state in
      tokens_til [TokEquals] state >>= fun tokens ->
      parse_type_expr tokens >>= fun type_sig ->
      let state = move state (length tokens) in
      expect_equals state >>= fun (expr, state) ->
      Ok (Declaration (name, type_sig, expr), state)
  | _ ->
      expect_equals state >>= fun (expr, state) ->
      Ok (Declaration (name, InferredType, expr), state)
  end

and parse_if_stmt state elifs =
  (* Condition *)
  tokens_til [TokWordThen] state >>= fun tokens ->
  parse_expr tokens >>= fun cond ->
  let state = move state (length tokens) in
  (* Consequence *)
  tokens_in_if_body state >>= fun tokens ->
  parse_stmt_list tokens >>= fun body ->
  let state = move state (length tokens) in
  match look state (-1) with
  | TokWordElif ->
      parse_if_stmt state (IfStmt { cond; body; elifs = []; final = [] } :: elifs)
  (* Parse final clause if it exists *)
  | TokWordElse ->
      tokens_til_end state >>= fun tokens ->
      parse_stmt_list tokens >>= fun final ->
      Ok (IfStmt { cond; body; elifs = rev elifs; final },
          move state (length tokens))
  | TokWordEnd ->
      Ok (IfStmt { cond; body; elifs = rev elifs; final = [] }, next state)
  | _ -> Error Unreachable

(* Get the tokens until the first matching elif, else, or end *)
and tokens_in_if_body state =
  let rec aux state depth accum =
    let tok = peek state in
    match tok with
    | TokEof -> Error MissingCloseDelim
    | TokWordIf | TokWordCase -> aux (next state) (depth + 1) (tok :: accum)
    | TokWordEnd | TokWordElif | TokWordElse when depth = 0 ->
        Ok (rev (TokEof :: accum))
    | TokWordEnd -> aux (next state) (depth - 1) (tok :: accum)
    | _ -> aux (next state) depth (tok :: accum)
  in
  aux state 0 []

(* Tokens until matching end *)
and tokens_til_end state =
  let rec aux state depth accum =
    let tok = peek state in
    match tok with
    | TokEof -> Error MissingCloseDelim
    | TokWordIf | TokWordCase -> aux (next state) (depth + 1) (tok :: accum)
    | TokWordEnd ->
        if depth = 0 then Ok (rev (TokEof :: accum))
        else aux (next state) (depth - 1) (tok :: accum)
    | _ -> aux (next state) depth (tok :: accum)
  in
  aux state 0 []

and parse_case_stmt state =
  (* Argument to match *)
  tokens_til [TokWordOf] state >>= fun tokens ->
  parse_expr tokens >>= fun argument ->
  let state = move state (length tokens) in
  (* Parse the labeled clauses *)
  parse_case_clauses state >>= fun (clauses, state) ->
  match peek state with
  (* Parse final clause if it exists *)
  | TokWordElse ->
      let state = next state in
      tokens_til_end state >>= fun tokens ->
      parse_stmt_list tokens >>= fun final ->
      let state = move state (length tokens) in
      begin match look state (-1) with
      | TokWordEnd -> Ok (CaseStmt { argument; clauses; final }, state)
      | tok -> Error (Expected ([TokWordEnd], tok))
      end
  | TokWordEnd -> Ok (CaseStmt { argument; clauses; final = [] }, next state)
  | tok -> Error (Expected ([TokWordElse; TokWordEnd], tok))

and parse_case_clauses state =
  let rec aux state depth clauses =
    match peek state with
    | TokWordElse | TokWordEnd when depth = 0 -> Ok (rev clauses, state)
    | TokWordCase | TokWordIf -> aux (next state) (depth + 1) clauses
    | TokWordEnd -> aux (next state) (depth - 1) clauses
    | TokWordLet -> (* Necessary to handle colons in declarations *)
        goto_next [TokColon; TokEquals] state >>= fun state ->
        aux (next state) depth clauses
    | TokColon ->
        (* Backtrack to find beginning of pattern *)
        goto_last [TokSemicolon; TokWordOf] state >>= fun state ->
        tokens_til [TokColon] state >>= fun tokens ->
        parse_pattern_list tokens >>= fun cases ->
        let state = move state (length tokens) in
        (* Collect tokens for clause body *)
        tokens_in TokWordLet TokColon state >>= fun tokens ->
        distance_last [TokSemicolon] state >>= fun dist ->
        let tokens = take (length tokens - dist) tokens in
        parse_stmt_list tokens >>= fun body ->
        aux
          (move state (length tokens))
          (depth)
          (CaseClause { cases; body } :: clauses)
    | _ -> aux (next state) depth clauses
  in
  aux state 0 []

and parse_stmt_list tokens =
  let rec aux state accum =
    match peek state with
    | TokEof -> Ok (rev accum) (* Finished *)
    | _ -> (* Parse next statement *)
        parse_stmt state >>= fun (stmt, state) ->
        aux state (stmt :: accum)
  in
  aux { stream = tokens; pos = 0 } []

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
      Ok (AliasDef (name, expr), move state (length tokens))
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
        Ok (rev accum, state)
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
  | FnType _ -> Ok (expr, move state (length tokens))
  | _ -> Error (ExpectedFnType expr)

and parse_fn_body state =
  match peek state with
  | TokReturnArrow ->
      tokens_til [TokSemicolon] (next state) >>= fun tokens ->
      parse_expr tokens >>= fun expr ->
      Ok ([ReturnStmt expr], move state (length tokens + 1))
  | TokBraceL ->
      tokens_in_braces (next state) >>= fun tokens ->
      parse_stmt_list tokens >>= fun body ->
      Ok (body, move state (length tokens + 2))
  | _ -> Error Unreachable

(* Type definitions *)
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
  Ok (TypeDef { name; params; value }, (move state (length tokens)))

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
      Ok (body, move state (length tokens))
  | tok -> Error (Expected ([TokBraceL], tok))

and parse_variant_list tokens =
  let rec aux state accum =
    match peek state with
    | TokEof -> Ok (rev accum) (* Finished *)
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
            aux (move state (length tokens))
                (ConstructorVariant (name, TupleType types) :: accum)
        (* Record constructor *)
        | TokBraceL ->
            tokens_in_braces (next state) >>= fun tokens ->
            parse_field_list tokens >>= fun fields ->
            aux (move state (length tokens))
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
      Ok (body, move state (length tokens))
  | tok -> Error (Expected ([TokBraceL], tok))

and parse_field_list tokens =
  let rec aux state accum =
    match peek state with
    | TokEof -> Ok (rev accum)
    | TokComma -> aux (next state) accum
    | TokIdent name ->
        let state = next state in
        begin match peek state with
        (* Normal field: identifier and a type *)
        | TokColon ->
            tokens_til_matching [TokComma; TokEof] (next state) >>= fun tokens ->
            parse_type_expr tokens >>= fun expr ->
            aux (move state (length tokens))
                (DeclField (name, expr) :: accum)
        (* Union field *)
        | TokBraceL ->
            tokens_in_braces (next state) >>= fun tokens ->
            parse_variant_list tokens >>= fun variants ->
            aux (move state (length tokens))
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
    | TokEof -> Ok (rev accum)
    | _ ->
        parse_top_level state >>= fun (def, state) ->
        loop state (def :: accum)
  in
  loop state []

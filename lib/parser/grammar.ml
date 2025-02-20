open Stdint
open Lexer

type node =
  (* Top-level *)
  | Import of string * string list
  | AliasDef of string * node
  | FnDef of
      { name : string
      ; params : string list list
      ; type_sig : node (* FnType *)
      ; body : node list (* statements *)
      }
  (* Type alias *)
  | TypeDef of
      { name : string
      ; params : string list
      ; value : node
      }
  (* Sum type *)
  | UnionDef of
      { name : string
      ; params : string list
      ; body : node list
      }
  | PureVariant of string
  | ConstructorVariant of string * node
  | SelfConstructorVariant of string
  | RecordVariant of string * node list
  (* Product type *)
  | RecordDef of
      { name : string
      ; params : string list
      ; body : node list
      }
  | DeclField of string * node
  | UnionField of string * node list

  (* Expressions *)
  | Unit
  | Reference of string
  | IntLiteral of uint64
  | FloatLiteral of uint64
  | BoolLiteral of bool
  | ListLiteral of node list
  | Tuple of node list
  | MatchExpr of node * (node * node) list
  | RecordLiteral of (string * node) list
  | Ternary of node * node * node
  | BinOp of token * node * node
  | UnOp of token * node
  | FnApp of string * node list
  | Lambda of string list * node
  (* Patterns *)
  | ConstExprPattern of node
  | CapturePattern of string
  | ConstructorPattern of string * node list

  (* Type expressions *)
  | InferredType
  | TypeReference of string
  | TupleType of node list
  | FnType of node list
  | ConstructorApp of string * node

  (* Statements *)
  | Declaration of string * node * node
  | Assignment of string * node
  | FnCall of node
  | ReturnStmt of node
  | Continue
  | Break
  | WhileLoop of
      { cond : node
      ; body : node list
      }
  | ForLoop of
      { capture : string
      ; iterator : node
      ; body : node list
      }
  | IfStmt of
      { cond : node
      ; body : node list (* statement list *)
      ; elifs : node list (* chained `IfStmt`s *)
      ; final : node list (* statement list *)
      }
  | CaseStmt of
      { argument : node
      ; clauses : node list
      ; final : node list
      }
  | CaseClause of
      { cases : node list (* pattern list *)
      ; body : node list (* statement list *)
      }

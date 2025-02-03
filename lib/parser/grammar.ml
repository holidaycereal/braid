[@@@warning "-duplicate-definitions"]
open Stdint

type ident = string

type program =
  { top_level : top_level list
  ; main_fn : fn_def
  }

and top_level =
  | AliasDef of alias_def
  | TypeDef of type_def
  | RecordDef of record_def
  | UnionDef of union_def
  | FnDef of fn_def
  | Import of import

and type_def =
  { name : ident
  ; type_params : ident list
  ; value : type_expr
  }
and alias_def =
  { name : ident
  ; params : ident list
  ; value : expr
  }

and record_def =
  { name : ident
  ; type_params : ident list
  ; body : field list
  }
and union_def =
  { name : ident
  ; type_params : ident list
  ; body : variant list
  }
and field =
  | DeclField of ident * type_expr
  | UnionField of union_def
  | RecordField of record_def
and variant =
  | PureValue of ident
  | Constructor of ident * type_expr
  | RecordVariant of record_def

and fn_def =
  { name : ident
  ; params : ident list list
  ; type_sig : type_expr list
  ; body : stmt list
  }

and import =
  | ModuleName of string
  | ModuleAsAlias of string * string
  | ItemsFromModule of string list * string

and stmt =
  | MutDecl of ident * type_expr * expr option
  | ImmutDecl of ident * type_expr * expr
  | Assignment of ident * expr
  | FnCall of ident * expr list
  | ReturnStmt of expr
  | Break of uint64
  | Continue
  | IfBlock of if_block
  | WithBlock of with_block
  | WhileBlock of while_block
  | ForBlock of for_block
  | SwitchBlock of switch_block

and if_block =
  { condition : expr
  ; body : stmt list
  ; else_clause : stmt list option
  }
and with_block =
  { pattern : pattern
  ; argument : expr
  ; body : stmt list
  ; else_clause : stmt list option
  }
and while_block =
  { condition : expr
  ; step : stmt option
  ; body : stmt list
  }
and for_block =
  { capture : ident
  ; iterator : expr
  ; body : stmt list
  }
and switch_block =
  { argument : expr
  ; clauses : switch_clause list
  ; default : stmt list option
  }
and switch_clause =
  { cases : pattern list
  ; body : stmt list
  }

and expr =
  | ValueLiteral of value
  | AliasRef of ident
  | VariableRef of ident
  | TupleLiteral of expr list
  | ListLiteral of expr list
  | RecordLiteral of record_literal
  | MatchExpr of match_expr
  | Ternary of expr * expr * expr
  | UnaryOpApp of unary_op * expr
  | BinaryOpApp of binary_op * expr * expr
  | FnApp of ident * expr list
  | Lambda of ident list * expr

and value =
  | IntValue of uint64 * width
  | UintValue of uint64 * width
  | FloatValue of uint64 * width
  | BoolValue of bool

and width = W8 | W16 | W32 | W64

and record_literal =
  { name : ident option
  ; fields : (ident * expr) list
  }
and match_expr =
  { argument : expr
  ; cases : (pattern * expr) list
  }

and pattern =
  | ConstantExpr of expr
  | Capture of ident
  | ConstructorPattern of ident * pattern list

and unary_op = BitNot | Not | Reference
and binary_op =
  | BitLsl | BitLsr | BitAsl | BitAsr | BitAnd | BitOr | BitXor
  | And | Or | Xor
  | CompEq | CompNe | CompLe | CompGe | CompLt | CompGt
  | Add | Sub | Mul | Div | Mod
  | MemberAccess | IndexAccess | Concat | Range

and type_expr =
  | Primitive of primitive
  | Generic of ident
  | TupleType of type_expr list
  | ArrayType of type_expr
  | FunctorApp of ident * type_expr list
  | TypeAliasRef of ident
  | RecordRef of ident
  | UnionRef of ident
  | Inferred

and primitive =
  | FixedUintType of width | UsizeType | UintType
  | FixedIntType of width | IsizeType | IntType
  | FixedFloatType of width | FloatType
  | BoolType

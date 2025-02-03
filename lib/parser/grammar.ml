open Stdint

type ident = string

type unary_op =
  | BitNot    (* a    -> a    *)
  | Not       (* bool -> bool *)
  | Reference (* a    -> &a   *)

type binary_op =
  | BitLsl | BitLsr | BitAsl | BitAsr (* a    -> a    -> a     *)
  | BitAnd | BitOr | BitXor
  | And | Or | Xor                    (* bool -> bool -> bool  *)
  | CompEq | CompNe | CompLe | CompGe (* ord  -> ord  -> bool  *)
  | CompLt | CompGt
  | Add | Sub | Mul | Div | Mod       (* num  -> num  -> num   *)
  | MemberAccess                      (* record -> member -> a *)
  | IndexAccess                       (* seq  -> uint -> a     *)
  | Concat                            (* seq  -> seq  -> seq   *)
  | Range                             (* int  -> int  -> iter  *)

type width = W8 | W16 | W32 | W64

type value = 
  | IntValue of uint64 * width
  | UintValue of uint64 * width
  | FloatValue of uint64 * width
  | BoolValue of bool

type expr =
  | ValueLiteral of value
  | AliasRef of ident
  | VarRef of ident
  | TupleLiteral of expr list
  | ListLiteral of expr list
  | RecordLiteral of {
    name : ident option;
    fields : (ident * expr) list;
  }
  | MatchExpr of {
    arg : expr;
    cases : (pattern * expr) list;
  }
  | Ternary of {
    cond : expr;
    left_value : expr;
    right_value : expr;
  }
  | UnaryOpApp of {
    operator : unary_op;
    operand : expr;
  }
  | BinaryOpApp of {
    operator : binary_op;
    lhs : expr;
    rhs : expr;
  }
  | FnApp of ident * expr list
  | Lambda of ident * expr
and pattern =
  | ConstantExpr of expr
  | Capture of ident
  | ConstructorPattern of {
    constructor : ident;
    args : pattern list;
  }

type primitive_type =
  | FixedUintType of width | UsizeType | UintType
  | FixedIntType of width | IsizeType | IntType
  | FixedFloatType of width | FloatType
  | BoolType

type type_expr =
  | PrimitiveType of primitive_type
  | GenericType of ident
  | TupleType of type_expr list
  | ArrayType of type_expr
  | FunctorApp of ident * type_expr list
  | TypeAliasRef of ident
  | RecordRef of ident
  | UnionRef of ident
  | InferredType

type mut_decl = {
  name : ident;
  type_sig : type_expr;
  value : expr option;
}

type immut_decl = {
  name : ident;
  type_sig : type_expr;
  value : expr;
}

type assign = {
  name : ident;
  value : expr;
}

type type_def = {
  name : ident;
  value : type_expr;
}

type alias_def = {
  name : ident;
  params : ident list;
  value : expr;
}

type field =
  | DeclField of mut_decl
  | UnionField of union_def
  | RecordField of record_def
and variant =
  | PureVariant of ident
  | ConstructorVariant of ident * type_expr
  | RecordVariant of record_def
and record_def = {
  record_name : ident;
  record_type_params : ident list;
  record_body : field list;
}
and union_def = {
  union_name : ident;
  union_type_params : ident list;
  union_body : variant list;
}

type stmt =
  | MutDecl of mut_decl
  | ImmutDecl of immut_decl
  | Assign of assign
  | AliasDef of alias_def
  | FnCall of ident * expr list
  | ReturnStmt of expr
  | Break of uint64
  | Continue
  | IfBlock of {
    cond : expr;
    body : stmt list;
    else_clause : stmt list option;
  }
  | WithBlock of {
    pattern : pattern;
    arg : expr;
    body : stmt list;
    else_clause : stmt list option;
  }
  | WhileBlock of {
    cond : expr;
    step : stmt option;
    body : stmt list;
  }
  | ForBlock of {
    capture : ident;
    iterator : expr;
    body : stmt list;
  }
  | SwitchBlock of {
    arg : expr;
    clauses : switch_clause list;
    default : stmt list option;
  }
and switch_clause = {
  switch_clause_cases : pattern list;
  switch_clause_body : stmt list;
}

type fn_def = {
  name : ident;
  params : (ident list) list;
  type_sig : type_expr list;
  body : stmt list;
}

type import_directive =
  | NormalImport of string
  | AsAliasImport of string * string
  | ExplicitImport of string list * string

type top_level =
  | AliasDef of alias_def
  | TypeDef of type_def
  | RecordDef of record_def
  | UnionDef of union_def
  | FnDef of fn_def
  | ImportDirective of import_directive

type program = {
  top_level : top_level list;
  main_fn : fn_def;
}

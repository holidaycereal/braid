open Stdint

type ident = string

type unary_op =
  | BitNot  (* a -> a    *)
  | Not     (* a -> bool *)

type binary_op =
  | BitLsl | BitLsr | BitAsl | BitAsr  (* a    -> a    -> a    *)
  | BitAnd | BitOr | BitXor            (* a    -> a    -> a    *)
  | And | Or | Xor                     (* bool -> bool -> bool *)
  | CompEq | CompNe | CompLe | CompGe  (* ord  -> ord  -> bool *)
  | Add | Sub | Mul | Div | Pow | Mod  (* num  -> num  -> num  *)
  | IndexAccess                        (* seq  -> uint -> a    *)
  | Concat                             (* seq  -> seq  -> seq  *)
  | Range                              (* int  -> int  -> iter *)

type width = W8 | W16 | W32 | W64

type expr =
  | IntValue of (uint64 * width)
  | UintValue of (uint64 * width)
  | FloatValue of (uint64 * width)
  | BoolValue of bool
  | AliasRef of ident
  | ConstRef of ident
  | VarRef of ident
  | MemberRef of {
    record : ident;
    member : ident;
  }
  | TupleExpr of expr list
  | ListExpr of expr list
  | Ternary of {
    cond : expr;
    left : expr;
    right : expr;
  }
  | MatchExpr of {
    target : expr;
    cases : (expr * expr) list;
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
  | FnApp of (ident * expr option)

type primitive =
  | U8 | U16 | U32 | U64 | Usize | Uint
  | I8 | I16 | I32 | I64 | Isize | Int
  | F32 | F64 | Float
  | Bool

type type_expr =
  | Primitive of primitive
  | Generic of ident
  | TupleType of type_expr list
  | ArrayApp of type_expr
  | FunctorApp of (ident * type_expr list)
  | TypeSig of (type_expr * type_expr)
  | TypeAliasRef of ident
  | RecordRef of ident
  | UnionRef of ident

type decl = {
  name : ident;
  type_expr : type_expr;
}

type assign = {
  name : ident;
  type_expr : type_expr option;
  value : expr;
}

type field =
  | Decl of decl
  | Assign of assign
  | Union of variant list
and variant =
  | Value of ident
  | Construct of (ident * type_expr)
  | Record of record_def
and record_def = {
  record_name : ident;
  record_body : field list;
}
and union_def = {
  union_name : ident;
  union_body : variant list;
}

type stmt =
  | Decl of decl
  | Assign of assign
  | Reassign of assign
  | ConstDef of assign
  | FnCall of (ident * expr option)
  | ReturnStmt of expr
  | Break
  | Continue
  | IfBlock of {
    initial_clause : (expr * stmt list);
    elif_clauses : (expr * stmt list) list;
    else_clause : stmt list option;
  }
  | SwitchBlock of {
    target : expr;
    cases : (expr list * stmt list) list;
    default : stmt list option;
  }
  | WhileBlock of {
    cond : expr;
    loop_stmt : stmt option;
    body : stmt list;
  }
  | ForInBlock of {
    capture : ident;
    iterator : expr;
    body : stmt list;
  }
  | ForToBlock of {
    init : assign;
    limit : expr;
    body : stmt list;
  }

type fn_def = {
  name : ident;
  params : (ident list) list;
  type_sig : type_expr;
  body : stmt list;
}

type import_directive =
  | Normal of ident
  | Include of ident
  | WithAlias of (ident * ident)
  | Explicit of (ident * ident list)

type top_level =
  | Decl of decl
  | Assign of assign
  | ConstDef of assign
  | AliasDef of (ident * expr)
  | TypeDef of (ident * type_expr)
  | RecordDef of record_def
  | UnionDef of union_def
  | FnDef of fn_def
  | ImportDirective of import_directive

type program = {
  top_level : top_level list;
  main_fn : fn_def;
}

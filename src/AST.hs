module AST where

import Token

data TopLevel
  = ExprDef String [Param] (Maybe TypeExpr) Expr
  | ConstDef String Expr
  | TypeDef String [String] TypeExpr
  | RecordDef String [String] [(String, TypeExpr)]
  | UnionDef String [String] [(String, Maybe TypeExpr)]
  | IfaceDef String [String] TypeExpr
  | TraitDef String [String] [TypeExpr]
  | ImplBlock String TypeExpr [TopLevel]

data Expr
  = Unit
  | Reference String
  | IntLiteral String
  | FloatLiteral String
  | StringLiteral String
  | CharLiteral String
  | TupleLiteral [Expr]
  | ListLiteral [Expr]

  | BinOp Expr BinOpKind Expr
  | UnOp UnOpKind Expr
  | FnApp Expr [Expr]

  | Lambda Param Expr
  | Ternary Expr Expr Expr
  | Match Expr [(Pattern, Expr)]
  | Matches Expr Pattern
  | Proc [Stmt]

data TypeExpr
  = UnitType
  | TypeReference String
  | ConstructorApp String [TypeExpr]
  | TupleType [TypeExpr]
  | FnType [TypeExpr]

data Stmt
  = ExprStmt Expr
  | ReturnStmt Expr
  | LetBinding Lvalue (Maybe TypeExpr) (Maybe Expr)
  | Declaration Lvalue (Maybe TypeExpr) (Maybe Expr)
  | Assignment Lvalue Expr
  | Block [Stmt]
  | IfStmt Expr [Stmt] (Maybe [Stmt])
  | LoopBlock [Stmt]
  | WhileLoop Expr [Stmt]
  | ForLoop Pattern Expr [Stmt]
  | BreakStmt
  | ContinueStmt

data Param
  = UnitParam
  | ParamSingle String
  | ParamTuple [Param]

data Lvalue
  = Name String
  | NameTuple [Lvalue]

data Pattern
  = Wildcard
  | Capture String
  | Value Expr
  | Constructor String Pattern
  | TuplePattern [Pattern]
  | Guarded Pattern Expr

data BinOpKind
  = ModuleAccess
  | MemberAccess
  | Mul | Div | Mod
  | Add | Sub
  | ExRange | InRange
  | Concatenate
  | Compose
  | Lt | Gt | Le | Ge
  | Eq | Ne
  | And
  | Or

data UnOpKind = Ref | Deref | Not | Negative

data OpKind = Binary BinOpKind | Unary UnOpKind

prec :: OpKind -> Int
prec (Binary Or)           = 1
prec (Binary And)          = 2
prec (Binary Eq)           = 3
prec (Binary Ne)           = 3
prec (Binary Lt)           = 4
prec (Binary Gt)           = 4
prec (Binary Le)           = 4
prec (Binary Ge)           = 4
prec (Binary Compose)      = 5
prec (Binary Concatenate)  = 6
prec (Binary InRange)      = 7
prec (Binary ExRange)      = 7
prec (Binary Add)          = 8
prec (Binary Sub)          = 8
prec (Binary Mul)          = 9
prec (Binary Div)          = 9
prec (Binary Mod)          = 9
prec (Unary Negative)      = 10
prec (Unary Not)           = 11
prec (Binary MemberAccess) = 12
prec (Unary Ref)           = 13
prec (Unary Deref)         = 13
prec (Binary ModuleAccess) = 14

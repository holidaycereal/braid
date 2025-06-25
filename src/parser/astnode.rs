// use crate::lexer::token::Token;
//
// pub enum TopLevelDef {
// 	ConstDef {
// 		name: String,
// 		value: Expr,
// 	},
// 	FnDef {
// 		name: String,
// 		params: Vec<LValue>,
// 		type_params: Vec<String>,
// 		type_sig: TypeExpr,
// 		body: Vec<Stmt>,
// 	},
// 	TypeDef {
// 		name: String,
// 		params: Vec<String>,
// 		value: TypeExpr,
// 	},
// 	UnionDef {
// 		name: String,
// 		params: Vec<String>,
// 		variants: Vec<Variant>,
// 	},
// 	RecordDef {
// 		name: String,
// 		params: Vec<String>,
// 		fields: Vec<Field>,
// 	},
// }
//
// pub enum Variant {
// 	PureVariant(String),
// 	SelfConstructor(String),
// 	Constructor {
// 		name: String,
// 		types: Vec<TypeExpr>,
// 	},
// 	RecordVariant {
// 		name: String,
// 		fields: Vec<Field>,
// 	},
// }
//
// pub struct Field {
// 	name: String,
// 	type_sig: TypeExpr,
// }
//
// pub enum Expr {
// 	Unit,
// 	Identifier(String),
// 	IntLiteral(u64),
// 	FloatLiteral(String),
// 	ListLiteral(Vec<Expr>),
// 	TupleExpr(Vec<Expr>),
// 	MatchExpr {
// 		argument: Box<Expr>,
// 		cases: Vec<(Vec<Pattern>, Expr)>,
// 	},
// 	RecordLiteral(Vec<(String, Expr)>),
// 	Ternary {
// 		condition: Box<Expr>,
// 		consequence: Box<Expr>,
// 		alternative: Box<Expr>,
// 	},
// 	BinOp {
// 		kind: Token,
// 		lhs: Box<Expr>,
// 		rhs: Box<Expr>,
// 	},
// 	UnOp {
// 		kind: Token,
// 		operand: Box<Expr>,
// 	},
// 	FnApp {
// 		name: String,
// 		args: Vec<Expr>,
// 	},
// 	Lambda {
// 		param: LValue,
// 		value: Box<Expr>,
// 	},
// }
//
// pub enum TypeExpr {
// 	Inferred,
// 	TupleType(Vec<TypeExpr>),
// 	FnType(Vec<TypeExpr>),
// 	ConstructorApp {
// 		name: String,
// 		args: Vec<TypeExpr>,
// 	},
// }
//
// pub enum Pattern {
// 	Wildcard,
// 	Capture(String),
// 	ConstExprPattern(Expr),
// 	TuplePattern(Vec<Pattern>),
// 	ConstructorPattern {
// 		name: String,
// 		args: Vec<Pattern>,
// 	},
// }
//
// pub enum LValue {
// 	Identifier(String),
// 	Nested(Vec<LValue>),
// }
//
// pub enum Stmt {
// 	Declaration {
// 		lvalue: LValue,
// 		type_sig: TypeExpr,
// 		rvalue: Option<Expr>,
// 	},
// 	Assignment {
// 		lvalue: LValue,
// 		rvalue: Expr,
// 	},
// 	FnCall(Expr),
// 	Return(Expr),
// 	Continue,
// 	Break,
// 	WhileLoop {
// 		condition: Expr,
// 		body: Vec<Stmt>,
// 	},
// 	ForLoop {
// 		capture: Pattern,
// 		iterator: Expr,
// 		guard: Option<Expr>,
// 		body: Vec<Stmt>,
// 	},
// 	IfStatement {
// 		condition: Expr,
// 		consequence: Vec<Stmt>,
// 		elifs: Vec<ElifClause>,
// 		fallback: Vec<Stmt>,
// 	},
// 	CaseStatement {
// 		argument: Expr,
// 		clauses: Vec<CaseClause>,
// 		fallback: Vec<Stmt>,
// 	},
// }
//
// pub struct ElifClause {
// 	pub condition: Expr,
// 	pub consequence: Vec<Stmt>,
// }
//
// pub struct CaseClause {
// 	pub patterns: Vec<Pattern>,
// 	pub body: Vec<Stmt>,
// }

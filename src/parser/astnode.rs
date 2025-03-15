use crate::lexer::token::Token;

pub enum TopLevelDef {
	ConstDef {
		name: String,
		value: Expr,
	},
	FnDef {
		name: String,
		params: Vec<Param>,
		type_params: Vec<String>,
		type_sig: TypeExpr,
		body: Vec<Stmt>,
	},
	TypeDef {
		name: String,
		params: Vec<String>,
		value: TypeExpr,
	},
	UnionDef {
		name: String,
		params: Vec<String>,
		variants: Vec<Variant>,
	},
	RecordDef {
		name: String,
		params: Vec<String>,
		fields: Vec<Field>,
	},
}

pub enum Param {
	NameList(Vec<String>),
	Nested(Vec<Param>),
}

pub enum Variant {
	PureVariant(String),
	SelfConstructor(String),
	Constructor {
		name: String,
		types: Vec<TypeExpr>,
	},
	RecordVariant {
		name: String,
		fields: Vec<Field>,
	},
}

pub enum Field {
	DeclField {
		name: String,
		type_sig: Box<Node>,
	},
	UnionField {
		name: String,
		variants: Vec<Node>,
	},
}

pub enum Expr {
	Unit,
	Identifier(String),
	IntLiteral(u64),
	FloatLiteral(String),
	ListLiteral(Vec<Node>),
	TupleExpr(Vec<Node>),
	MatchExpr {
		argument: Box<Node>,
		cases: Vec<(Node, Node)>,
	},
	RecordLiteral(Vec<(String, Node)>),
	Ternary {
		condition: Box<Node>,
		consequence: Box<Node>,
		alternative: Box<Node>,
	},
	BinOp {
		kind: Token,
		lhs: Box<Node>,
		rhs: Box<Node>,
	},
	UnOp {
		kind: Token,
		operand: Box<Node>,
	},
	FnApp {
		name: String,
		args: Vec<Node>,
	},
	Lambda {
		params: Vec<String>,
		value: Box<Node>,
	},
}

pub enum Pattern {
	ConstExprPattern(Box<Node>),
	ConstructorPattern {
		name: String,
		args: Vec<Node>,
	},
}

pub enum TypeExpr {
	Inferred,
	TupleType(Vec<Node>),
	FnType(Vec<Node>),
	ConstructorApp {
		name: String,
		args: Vec<Node>,
	},
}

pub enum Stmt {
	Declaration {
		names: Vec<String>,
		type_sig: Box<Node>,
		value: Option<Box<Node>>,
	},
	Assignment {
		lvalue: Box<Node>,
		rvalue: Box<Node>,
	},
	FnCall(Box<Node>),
	Return(Box<Node>),
	Continue,
	Break,
	WhileLoop {
		condition: Box<Node>,
		body: Vec<Node>,
	},
	ForLoop {
		captures: Vec<String>,
		iterator: Box<Node>,
		body: Vec<Node>,
	},
	IfStatement {
		condition: Box<Node>,
		consequence: Vec<Node>,
		elifs: Vec<Node>,
		fallback: Vec<Node>,
	},
	ElifClause {
		condition: Box<Node>,
		consequence: Vec<Node>,
	},
	CaseStatement {
		argument: Box<Node>,
		clauses: Vec<Node>,
		fallback: Vec<Node>,
	},
	CaseClause {
		pattern: Vec<Node>,
		body: Vec<Node>,
	},
}

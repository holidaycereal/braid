use crate::lexer::token::Token;

pub enum Node {
	// Top-level definitions
	ConstDef {
		name: String,
		value: Box<Node>,
	},
	FnDef {
		name: String,
		params: Vec<Vec<String>>,
		type_sig: Box<Node>,
		statements: Vec<Node>,
	},
	TypeDef {
		name: String,
		params: Vec<String>,
		value: Box<Node>,
	},
	UnionDef {
		name: String,
		params: Vec<String>,
		variants: Vec<Node>,
	},
	RecordDef {
		name: String,
		params: Vec<String>,
		fields: Vec<Node>,
	},

	// Variants
	PureVariant(String),
	SelfConstructor(String),
	Constructor {
		name: String,
		types: Vec<Node>,
	},
	RecordVariant {
		name: String,
		fields: Vec<Node>,
	},

	// Fields
	DeclField {
		name: String,
		type_sig: Box<Node>,
	},
	UnionField {
		name: String,
		variants: Vec<Node>,
	},

	// Expressions
	Unit,
	Identifier(String),
	IntLiteral(u64),
	FloatLiteral(String),
	BoolLiteral(bool),
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

	// Patterns
	ConstExprPattern(Box<Node>),
	ConstructorPattern {
		name: String,
		args: Vec<Node>,
	},

	// Type expressions
	Inferred,
	TupleType(Vec<Node>),
	FnType(Vec<Node>),
	ConstructorApp {
		name: String,
		args: Vec<Node>,
	},

	// Statements
	Declaration {
		names: Vec<String>,
		type_sig: Box<Node>,
		value: Box<Node>,
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
	CaseStatement {
		argument: Box<Node>,
		clauses: Vec<Node>,
		fallback: Vec<Node>,
	},
	CaseClause {
		cases: Vec<Node>,
		body: Vec<Node>,
	},
}

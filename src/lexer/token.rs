#[derive(Clone, Eq, PartialEq)]
pub enum Token {
	Unknown(char),
	Identifier(String),
	// Literals
	StringLiteral(String),
	CharLiteral(String),
	IntLiteral(u64),
	FloatLiteral(String),
	// Keywords
	WordLet,
	WordReturn,
	WordIf,
	WordThen,
	WordElse,
	WordElif,
	WordCase,
	WordOf,
	WordEnd,
	WordWhile,
	WordFor,
	WordIn,
	WordWhere,
	WordDo,
	WordDone,
	WordBreak,
	WordContinue,
	WordMatch,
	WordTest,
	WordConst,
	WordType,
	WordDef,
	WordFn,
	WordRecord,
	WordUnion,
	WordTrait,
	WordImpl,
	WordAnd,
	WordOr,
	WordXor,
	WordNot,
	// Multi-character symbols
	InclusiveRange,  // ..=
	ConcatAssign,    // ++=
	ExclusiveRange,  // ..
	Concat,          // ++
	CompLe,          // <=
	CompGe,          // >=
	TestEq,          // ==
	TestNe,          // !=
	Arrow,           // ->
	FatArrow,        // =>
	DoublePipe,      // ||
	FwdCompose,      // >>
	ModuleAccess,    // ::
	AddAssign,       // +=
	SubAssign,       // -=
	MulAssign,       // *=
	DivAssign,       // /=
	ModAssign,       // %=
	InfixAssign,     // |=
	// One-character symbols
	ParenL,
	ParenR,
	BracketL,
	BracketR,
	BraceL,
	BraceR,
	Dot,
	Comma,
	Semicolon,
	Colon,
	Equals,
	Pipe,
	Bang,
	Ampersand,
	Hash,
	Caret,
	Tilde,
	Minus,
	Plus,
	Star,
	Slash,
	Percent,
	Less,
	Greater,
}

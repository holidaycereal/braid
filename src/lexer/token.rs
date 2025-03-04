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
	WordTrue,
	WordFalse,
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
	WordDo,
	WordDone,
	WordBreak,
	WordContinue,
	WordMatch,
	WordWhen,
	WordConst,
	WordType,
	WordFn,
	WordRecord,
	WordUnion,
	WordTrait,
	WordImpl,
	WordAnd,
	WordOr,
	WordXor,
	WordNot,
	// Two-character symbols
	CompLe,          // <=
	CompGe,          // >=
	TestEq,          // ==
	TestNe,          // !=
	Arrow,           // ->
	FatArrow,        // =>
	DoublePipe,      // ||
	FwdCompose,      // >>
	ExclusiveRange,  // ..
	InclusiveRange,  // .*
	ModuleAccess,    // ::
	Concat,          // ++
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
	Minus,
	Plus,
	Star,
	Slash,
	Percent,
	Less,
	Greater,
}

pub enum Token {
	EofToken,
	Unknown(u8),
	Identifier(String),
	// Literals
	StringLiteral(String),
	CharLiteral(String),
	IntLiteral(u64),
	FloatLiteral(String),
	// Keywords
	TrueLiteral,
	FalseLiteral,
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

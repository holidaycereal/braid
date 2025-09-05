#[derive(Debug)]
#[allow(dead_code)] // rust dead code analysis ignores derived impls
pub enum SyntaxErrorKind {
    // lexer errors
	UnknownCharacter(char),
    UnterminatedLiteral,
	UnterminatedComment,
    UnterminatedNumericLiteral,
    InvalidNumericLiteral,

	// Expected(Vec<Token>, Token),
	// ExpectedIdentifier(Token),
	// ExpectedIdentifierOr(Vec<Token>, Token),
	// ExpectedTopLevel(Token),
	// ExpectedStatement(Token),
	// UnexpectedEof,
	// LastTerminatorNotFound,
}

pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub line: usize,
    pub column: usize,
}

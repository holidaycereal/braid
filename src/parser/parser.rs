// use crate::lexer::{ lexer::Lexer, token::Token };
// use crate::parser::astnode::*;

#[derive(Debug)]
#[allow(dead_code)] // rust dead code analysis ignores derived impls
pub enum SyntaxError {
    // lexer errors
	UnknownCharacter(char),
    UnterminatedLiteral,
	UnterminatedComment,

	// Expected(Vec<Token>, Token),
	// ExpectedIdentifier(Token),
	// ExpectedIdentifierOr(Vec<Token>, Token),
	// ExpectedTopLevel(Token),
	// ExpectedStatement(Token),
	// UnexpectedEof,
	// LastTerminatorNotFound,
}

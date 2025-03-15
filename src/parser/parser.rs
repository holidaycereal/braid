use crate::lexer::{ lexer::Lexer, token::Token };
use crate::parser::astnode::*;

pub enum ParserError {
	UnknownCharacter(char), // Propagate lexer error
	Expected(Vec<Token>, Token), // Expected vs actual
	ExpectedIdentifier(Token),
	ExpectedIdentifierOr(Vec<Token>, Token),
	ExpectedTopLevel(Token),
	ExpectedStatement(Token),
	UnexpectedEof,
	LastTerminatorNotFound,
}

// Given an invalid token, produce an appropriate error
fn handle_bad_token<T>(token: Option<Token>, expected: Vec<Token>) -> Result<T, ParserError> {
	match token {
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::Expected(expected, token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

fn handle_bad_token_or_ident<T>(token: Option<Token>, expected: Vec<Token>) -> Result<T, ParserError> {
	match token {
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::ExpectedIdentifierOr(expected, token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

// Consume a token and fail if it's not the expected token
fn expect_token(expected: Token, lexer: &mut Lexer) -> Result<(), ParserError> {
	match lexer.consume() {
		Some(token) if token == expected => Ok(()),
		token => handle_bad_token(token, vec![expected]),
	}
}

fn expect_oneof(expected: Vec<Token>, lexer: &mut Lexer) -> Result<Token, ParserError> {
	match lexer.consume() {
		Some(token) if expected.contains(&token) => Ok(token),
		token => handle_bad_token(token, expected),
	}
}

// Expect an identifier and return its name or fail
fn parse_identifier(lexer: &mut Lexer) -> Result<String, ParserError> {
	match lexer.consume() {
		Some(Token::Identifier(name)) => Ok(name),
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::ExpectedIdentifier(token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

// Parse a list of comma-separated identifiers in parens or brackets
fn parse_idents_til(delim: Token, lexer: &mut Lexer) -> Result<Vec<String>, ParserError> {
	let mut names: Vec<String> = Vec::new();
	loop {
		match lexer.consume() {
			Some(tok) if tok == delim => return Ok(names),
			Some(Token::Identifier(name)) => { names.push(name); },
			Some(Token::Comma) => {},
			tok => return handle_bad_token_or_ident(tok, vec![Token::Comma]),
		}
	}
}

// Consume all the tokens until a delimiter and construct a vector from them
fn tokens_til(delim: Token, lexer: &mut Lexer) -> Result<Vec<Token>, ParserError> {
	let mut tokens: Vec<Token> = Vec::new();
	while let Some(token) = lexer.consume() {
		if token == delim { return Ok(tokens); }
		tokens.push(token);
	}
	Err(ParserError::UnexpectedEof)
}

fn tokens_til_oneof(delims: Vec<Token>, lexer: &mut Lexer) -> Result<Vec<Token>, ParserError> {
	let mut tokens: Vec<Token> = Vec::new();
	while let Some(token) = &lexer.consume() {
		if delims.contains(token) { return Ok(tokens); }
		tokens.push(token.clone());
	}
	Err(ParserError::UnexpectedEof)
}

// Parse an imperative block terminated by a delimiter
fn parse_block(lexer: &mut Lexer, delims: Vec<Token>) -> Result<Vec<Stmt>, ParserError> {
	let mut statements: Vec<Stmt> = Vec::new();
	while let Some(token) = &lexer.current {
		if delims.contains(token) { return Ok(statements); }
		let statement = parse_statement(lexer)?;
		statements.push(statement);
	}
	Err(ParserError::UnexpectedEof)
}

fn parse_loop_body(lexer: &mut Lexer) -> Result<Vec<Stmt>, ParserError> {
	parse_block(lexer, vec![Token::WordDone])
}

fn parse_if_body(lexer: &mut Lexer) -> Result<Vec<Stmt>, ParserError> {
	parse_block(lexer, vec![Token::WordElse, Token::WordElif, Token::WordEnd])
}

// Parse a statement
fn parse_statement(lexer: &mut Lexer) -> Result<Stmt, ParserError> {
	match lexer.consume() {
		// Declaration
		Some(Token::WordLet) => {
			let lvalue_tokens = tokens_til_oneof(vec![
				Token::Equals, Token::Colon
			], lexer)?;
			let lvalue = parse_lvalue(lvalue_tokens)?;

			let type_sig = match lexer.consume() {
				Some(Token::Equals) => TypeExpr::Inferred,
				_ => { // Can only be colon
					let tokens = tokens_til(Token::Equals, lexer)?;
					parse_type_expr(tokens)?
				},
			};

			let rvalue_tokens = tokens_til(Token::Semicolon, lexer)?;
			let rvalue = parse_expr(rvalue_tokens)?;

			Ok(Stmt::Declaration { lvalue, type_sig, rvalue })
		},

		// Assignment, function call TODO

		// Return statement
		Some(Token::WordReturn) => {
			let tokens = tokens_til(Token::Semicolon, lexer)?;
			let value = parse_expr(tokens)?;
			Ok(Stmt::Return(value))
		},

		// Break and continue
		Some(Token::WordBreak | Token::WordContinue) => {
			let token = lexer.current.unwrap();
			expect_token(Token::Semicolon, lexer)?;
			Ok(if token == Token::WordBreak { Stmt::Break } else { Stmt::Continue })
		},

		// While loop
		Some(Token::WordWhile) => {
			let tokens = tokens_til(Token::WordDo, lexer)?;
			let condition = parse_expr(tokens)?;

			let body = parse_loop_body(lexer)?;
			Ok(Stmt::WhileLoop { condition, body })
		},

		// For loop
		Some(Token::WordFor) => {
			let pattern_tokens = tokens_til(Token::WordIn, lexer)?;
			let capture = parse_pattern(pattern_tokens)?;

			let iterator_tokens = tokens_til_oneof(vec![
				Token::WordDo, Token::WordWhere
			], lexer)?;
			let iterator = parse_expr(iterator_tokens)?;

			let guard: Option<Expr> = match lexer.current {
				Some(Token::WordWhere) => {
					let guard_tokens = tokens_til(Token::WordDo, lexer)?;
					Some(parse_expr(guard_tokens))?
				},
				_ => None,
			};

			let body = parse_loop_body(lexer)?;

			Ok(Stmt::ForLoop { capture, iterator, guard, body })
		},

		// If statement
		Some(Token::WordIf) => {
			let tokens = tokens_til(Token::WordThen, lexer)?;
			let condition = parse_expr(tokens)?;

			let consequence = parse_if_body(lexer)?;

			// Elif clauses
			let mut elifs: Vec<ElifClause> = Vec::new();
			loop {
				match lexer.current {
					Some(Token::WordElif) => {
						let tokens = tokens_til(Token::WordThen, lexer)?;
						let condition = parse_expr(tokens)?;

						let consequence = parse_if_body(lexer)?;
						elifs.push(ElifClause { condition, consequence });
					},
					Some(Token::WordElse | Token::WordEnd) => { break; },
					tok => return handle_bad_token(tok, vec![
						Token::WordElif, Token::WordElse, Token::WordEnd
					]),
				}
			}

			// Else clause
			let fallback = match lexer.current {
				Some(Token::WordElse) => parse_block(lexer, vec![Token::WordEnd])?,
				_ => Vec::new(),
			};

			Ok(Stmt::IfStatement { condition, consequence, elifs, fallback })
		},

		// Case statement
		Some(Token::WordCase) => {
			let arg_tokens = tokens_til(Token::WordOf, lexer)?;
			let argument = parse_expr(arg_tokens)?;

			// Clauses
			let mut clauses: Vec<CaseClause> = Vec::new();
			while match lexer.current { // While we are still parsing cases...
				Some(Token::WordElse | Token::WordEnd) => false,
				_ => true, // Can only be a semicolon after the first iteration
			} {
				// Parse the patterns to match against
				let pattern_list_tokens = tokens_til(Token::Colon, lexer)?;
				let patterns = parse_pattern_list(pattern_list_tokens)?;

				// Parse the statements into an inner accumulator, stopping
				// once it doesn't look like a statement
				let mut body: Vec<Stmt> = Vec::new();
				while let Ok(stmt) = parse_statement(lexer) { body.push(stmt); }

				// Append it all to the outer accumulator and move the cursor
				// back to the end of the last statement, so we can either
				// continue parsing clauses, or finish the case statement
				clauses.push(CaseClause { patterns, body });
				lexer.goto_last_terminator()?;
			}

			// Else clause
			let fallback = match lexer.current {
				Some(Token::WordElse) => parse_block(lexer, vec![Token::WordEnd])?,
				_ => Vec::new(),
			};

			Ok(Stmt::CaseStatement { argument, clauses, fallback })
		},

		// Fail
		Some(tok) => Err(ParserError::ExpectedStatement(tok)),
		None => Err(ParserError::UnexpectedEof),
	}
}

// Top-level parsing function
pub fn parse(input: &str) -> Result<Vec<TopLevelDef>, ParserError> {
	let mut lexer = Lexer::new(input);
	let mut defs: Vec<TopLevelDef> = Vec::new();

	while let Some(token) = lexer.consume() {
		match token {
			// Constant expression definition
			Token::WordConst => {
				let name = parse_identifier(&mut lexer)?;
				expect_token(Token::Equals, &mut lexer)?;
				let tokens = tokens_til(Token::Semicolon, &mut lexer)?;
				let value = parse_expr(&tokens)?;
				defs.push(TopLevelDef::ConstDef { name, value });
			},

			// Function definition
			Token::WordDef => {
				let name = parse_identifier(&mut lexer)?;

				// Parameters
				let mut params: Vec<LValue> = Vec::new();
				while let Some(Token::ParenL) = lexer.consume() {
					let inner_params = parse_lvalue(&mut lexer)?;
					params.push(inner_params);
				}

				// Type parameters
				let type_params = parse_type_params(&mut lexer)?;

				// Type signature
				let type_sig = match lexer.current {
					Some(Token::Colon) => {
						let tokens = tokens_til_oneof(vec![
							Token::Equals, Token::BraceL
						], &mut lexer)?;
						parse_type_expr(tokens)?
					},
					Some(Token::Equals | Token::BraceL) => TypeExpr::Inferred,
					tok => return handle_bad_token(tok, vec![
						Token::Colon, Token::Equals, Token::BraceL
					]),
				};

				// Function body
				let body = match lexer.current {
					// Single expression
					Some(Token::Equals) => {
						let tokens = tokens_til(Token::Semicolon, &mut lexer)?;
						let expr = parse_expr(tokens)?;
						vec![Stmt::Return(expr)]
					},
					// Multiple statements
					Some(Token::BraceL) =>
						parse_block(&mut lexer, vec![Token::BraceR])?,
					tok => return handle_bad_token(tok, vec![
						Token::Equals, Token::BraceL
					]),
				};

				defs.push(TopLevelDef::FnDef {
					name, params, type_params, type_sig, body
				});
			},

			// Type alias definition
			Token::WordType => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::Equals, &mut lexer)?;
				let tokens = tokens_til(Token::Semicolon, &mut lexer)?;
				let value = parse_type_expr(tokens)?;
				defs.push(TopLevelDef::TypeDef { name, params, value });
			},

			// Union type definition
			Token::WordUnion => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::BraceL, &mut lexer)?;
				let variants = parse_variant_list(&mut lexer)?;
				defs.push(TopLevelDef::UnionDef { name, params, variants });
			},

			// Record type definition
			Token::WordRecord => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::BraceL, &mut lexer)?;
				let fields = parse_field_list(&mut lexer)?;
				defs.push(TopLevelDef::RecordDef { name, params, fields });
			},

			// Fail
			Token::Unknown(c) => return Err(ParserError::UnknownCharacter(c)),
			tok => return Err(ParserError::ExpectedTopLevel(tok)),
		}
	}

	Ok(defs)
}

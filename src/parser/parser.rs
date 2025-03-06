use crate::lexer::{ lexer::Lexer, token::Token };
use crate::parser::astnode::Node;

pub enum ParserError {
	UnknownCharacter(char), // Propagate lexer error
	Expected(Vec<Token>, Token), // Expected vs actual
	ExpectedIdentifier(Token),
	ExpectedIdentifierOr(Vec<Token>, Token),
	ExpectedTopLevel(Token),
	ExpectedStatement(Token),
	UnexpectedEof,
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

fn tokens_til_semicolon(lexer: &mut Lexer) -> Result<Vec<Token>, ParserError> {
	tokens_til(Token::Semicolon, lexer)
}

fn tokens_til_equals(lexer: &mut Lexer) -> Result<Vec<Token>, ParserError> {
	tokens_til(Token::Equals, lexer)
}

// Parse a statement
fn parse_statement(lexer: &mut Lexer) -> Result<Node, ParserError> {
	match lexer.consume() {
		// Declaration
		Some(Token::WordLet) => {
			let names = match lexer.consume() {
				Some(Token::Identifier(name)) => vec![name],
				Some(Token::ParenL) => parse_idents_til(Token::ParenR, lexer)?,
				tok => return handle_bad_token_or_ident(tok, vec![Token::ParenL]),
			};
			let type_sig = match lexer.consume() {
				Some(Token::Equals) => Node::Inferred,
				Some(Token::Colon) => {
					let tokens = tokens_til_equals(lexer)?;
					parse_type_expr(tokens)?
				},
				tok => return handle_bad_token(tok, vec![Token::Equals, Token::Colon]),
			};
			let tokens = tokens_til_semicolon(lexer)?;
			let value = parse_expr(tokens)?;
			Ok(Node::Declaration { names, type_sig, value })
		},

		// Assignment, function call TODO

		// Return statement
		Some(Token::WordReturn) => {
			let tokens = tokens_til_semicolon(lexer)?;
			let value = parse_expr(tokens)?;
			Ok(Node::Return(value))
		},

		// Break and continue
		Some(Token::WordBreak | Token::WordContinue) => {
			let token = lexer.current.unwrap();
			expect_token(Token::Semicolon, lexer)?;
			Ok(if token == Token::WordBreak { Node::Break } else { Node::Continue })
		},

		// While loop
		Some(Token::WordWhile) => {
			let tokens = tokens_til(Token::WordDo, lexer)?;
			let condition = parse_expr(tokens)?;
			let body = parse_done_block(lexer)?;
			Ok(Node::WhileLoop { condition, body })
		},

		// For loop
		Some(Token::WordFor) => {
			let captures = match lexer.consume() {
				Some(Token::Identifier(name)) => vec![name],
				Some(Token::ParenL) => parse_idents_til(Token::ParenR, lexer)?,
				tok => return handle_bad_token_or_ident(tok, vec![Token::ParenL]),
			};
			let tokens = tokens_til(Token::WordDo, lexer)?;
			let iterator = parse_expr(tokens)?;
			let body = parse_done_block(lexer)?;
			Ok(Node::ForLoop { captures, iterator, body })
		},

		// If statement
		Some(Token::WordIf) => {
			let tokens = tokens_til(Token::WordThen, lexer)?;
			let condition = parse_expr(tokens)?;
			let consequence = parse_if_block(lexer)?;

			// Elif clauses
			let mut elifs: Vec<Node> = Vec::new();
			loop {
				match lexer.current {
					Some(Token::WordElif) => {
						let tokens = tokens_til(Token::WordThen, lexer)?;
						let condition = parse_expr(tokens)?;
						let consequence = parse_if_block(lexer)?;
						elifs.push(Node::ElifClause { condition, consequence });
					},
					Some(Token::WordElse | Token::WordEnd) => { break; },
					tok => return handle_bad_token(tok, vec![
						Token::WordElif, Token::WordElse, Token::WordEnd
					]),
				}
			}

			// Else clause
			let fallback = if lexer.current.unwrap() == Token::WordElse {
				parse_block(lexer, vec![Token::WordEnd])?
			} else {
				Vec::new()
			};

			Ok(Node::IfStatement { condition, consequence, elifs, fallback })
		},

		// Case statement
		Some(Token::WordCase) => {
			let tokens = tokens_til(Token::WordOf, lexer)?;
			let argument = parse_expr(tokens)?;

			// Clauses
			let mut clauses: Vec<Node> = Vec::new();
			while match lexer.current {
				Some(Token::WordElse | Token::WordEnd) => false,
				_ => true,
			} {
				let tokens = tokens_til(Token::Colon, lexer)?;
				let cases = parse_pattern(tokens)?;

				let mut body: Vec<Node> = Vec::new();
				while let Ok(stmt) = parse_statement(lexer) {
					body.push(stmt);
				}
				clauses.push(Node::CaseClause { cases, body });
				lexer.goto_last_terminator();
			}

			// Else clause
			let fallback = if lexer.current.unwrap() == Token::WordElse {
				parse_block(lexer, vec![Token::WordEnd])?
			} else {
				Vec::new()
			};

			Ok(Node::CaseStatement { argument, clauses, fallback })
		},

		// Fail
		Some(tok) => Err(ParserError::ExpectedStatement(tok)),
		None => Err(ParserError::UnexpectedEof),
	}
}

// Parse an imperative block terminated by a delimiter
fn parse_block(lexer: &mut Lexer, delims: Vec<Token>) -> Result<Vec<Node>, ParserError> {
	let mut statements: Vec<Node> = Vec::new();
	while let Some(token) = &lexer.current {
		if delims.contains(token) { return Ok(statements); }
		let statement = parse_statement(lexer)?;
		statements.push(statement);
	}
	Err(ParserError::UnexpectedEof)
}

fn parse_done_block(lexer: &mut Lexer) -> Result<Vec<Node>, ParserError> {
	parse_block(lexer, vec![Token::WordDone])
}

fn parse_if_block(lexer: &mut Lexer) -> Result<Vec<Node>, ParserError> {
	parse_block(lexer, vec![
		Token::WordElse, Token::WordElif, Token::WordEnd
	])
}

// Top-level parsing function
pub fn parse(input: &str) -> Result<Vec<Node>, ParserError> {
	let mut lexer = Lexer::new(input);
	let mut nodes: Vec<Node> = Vec::new();

	while let Some(token) = lexer.consume() {
		match token {
			// Constant expression definition
			Token::WordConst => {
				let name = parse_identifier(&mut lexer)?;
				expect_token(Token::Equals, &mut lexer)?;
				let tokens = tokens_til_semicolon(&mut lexer)?;
				let value = parse_expr(&tokens)?;
				nodes.push(Node::ConstDef { name, value });
			},

			// Function definition
			Token::WordDef => {
				let name = parse_identifier(&mut lexer)?;

				// Parse parameters
				let mut params: Vec<Vec<String>> = Vec::new();
				while let Some(Token::ParenL) = lexer.consume() {
					let inner_params = parse_idents_til(Token::ParenR, &mut lexer)?;
					params.push(inner_params);
				}

				// Type signature
				let type_sig = match lexer.current {
					Some(Token::Colon) => parse_type_expr(&mut lexer)?,
					Some(Token::Equals | Token::BraceL) => Node::Inferred,
					tok => return handle_bad_token(tok, vec![
						Token::Colon, Token::Equals, Token::BraceL
					]),
				};

				// Function body
				let body = match lexer.current {
					Some(Token::Equals) => {
						let tokens = tokens_til_semicolon(&mut lexer)?;
						let expr = parse_expr(&tokens)?;
						vec![Node::Return(expr)]
					},
					Some(Token::BraceL) => parse_fn_body(&mut lexer)?,
					tok => return handle_bad_token(tok, vec![
						Token::Equals, Token::BraceL
					]),
				};

				nodes.push(Node::FnDef { name, params, type_sig, body });
			},

			// Type alias definition
			Token::WordType => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::Equals, &mut lexer)?;
				let tokens = tokens_til_semicolon(&mut lexer)?;
				let value = parse_type_expr(&tokens)?;
				nodes.push(Node::TypeDef { name, params, value });
			},

			// Union type definition
			Token::WordUnion => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::BraceL, &mut lexer)?;
				let variants = parse_variant_list(&mut lexer)?;
				nodes.push(Node::UnionDef { name, params, variants });
			},

			// Record type definition
			Token::WordRecord => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::BraceL, &mut lexer)?;
				let fields = parse_field_list(&mut lexer)?;
				nodes.push(Node::RecordDef { name, params, fields });
			},

			// Fail
			Token::Unknown(c) => return Err(ParserError::UnknownCharacter(c)),
			tok => return Err(ParserError::ExpectedTopLevel(tok)),
		}
	}

	Ok(nodes)
}

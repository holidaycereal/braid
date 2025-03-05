use crate::lexer::{ lexer::Lexer, token::Token };
use crate::parser::astnode::Node;

pub enum ParserError {
	UnknownCharacter(char), // Propagate lexer error
	Expected(Vec<Token>, Token), // Expected vs actual
	ExpectedIdentifier(Token),
	ExpectedIdentifierOr(Vec<Token>, Token),
	ExpectedTopLevel(Token),
	UnexpectedEof,
}

// Given an invalid token, produce an appropriate error
fn handle_bad_token<T>(token: Option<Token>, expected: Vec<Token>) ->
Result<T, ParserError> {
	match token {
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::Expected(expected, token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

fn handle_bad_token_or_ident<T>(token: Option<Token>, expected: Vec<Token>) ->
Result<T, ParserError> {
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

fn expect_oneof(expected: Vec<Token>, lexer: &mut Lexer) ->
Result<Token, ParserError> {
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
fn parse_idents_til(delim: Token, lexer: &mut Lexer) ->
Result<Vec<String>, ParserError> {
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
fn tokens_til(delims: Vec<Token>, lexer: &mut Lexer) ->
Result<Vec<Token>, ParserError> {
	let mut tokens: Vec<Token> = Vec::new();
	while let Some(token) = lexer.consume() {
		if delims.contains(&token) { return Ok(tokens); }
		tokens.push(token);
	}
	Err(ParserError::UnexpectedEof)
}

fn tokens_til_semicolon(lexer: &mut Lexer) -> Result<Vec<Token>, ParserError> {
	tokens_til(vec![Token::Semicolon], lexer)
}

// Parse an imperative block terminated by a delimiter
fn parse_block(lexer: &mut Lexer, delim: &Token) ->
Result<Vec<Node>, ParserError> {
	let mut statements: Vec<Node> = Vec::new();
	while lexer.cur != Some(delim.clone()) {
		let statement = parse_statement(lexer)?;
		statements.push(statement);
	}
	Ok(statements)
}

fn parse_end_block(lexer: &mut Lexer) -> Result<Vec<Node>, ParserError> {
	parse_block(lexer, &Token::WordEnd)
}

fn parse_done_block(lexer: &mut Lexer) -> Result<Vec<Node>, ParserError> {
	parse_block(lexer, &Token::WordDone)
}

fn parse_stop_block(lexer: &mut Lexer) -> Result<Vec<Node>, ParserError> {
	parse_block(lexer, &Token::WordStop)
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
				let type_sig = match lexer.cur {
					Some(Token::Colon) => parse_type_expr(&mut lexer)?,
					Some(Token::Equals | Token::WordProc) => Node::Inferred,
					tok => return handle_bad_token(tok, vec![
						Token::Colon, Token::Equals, Token::WordProc
					]),
				};

				// Function body
				let body = match lexer.cur {
					Some(Token::Equals) => {
						let tokens = tokens_til_semicolon(&mut lexer)?;
						let expr = parse_expr(&tokens)?;
						vec![Node::Return(expr)]
					},
					Some(Token::WordProc) => parse_stop_block(&mut lexer)?,
					tok => return handle_bad_token(tok, vec![
						Token::Equals, Token::WordProc
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

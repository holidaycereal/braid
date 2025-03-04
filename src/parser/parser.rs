use crate::lexer::{ lexer::Lexer, token::Token };
use crate::parser::astnode::Node;

pub enum ParserError {
	UnknownCharacter(char),
	Expected(Vec<Token>, Token),
	ExpectedIdentifier(Token),
	ExpectedIdentifierOr(Vec<Token>, Token),
	UnexpectedEof,
}

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

fn parse_identifier(lexer: &mut Lexer) -> Result<String, ParserError> {
	match lexer.consume() {
		Some(Token::Identifier(name)) => Ok(name),
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::ExpectedIdentifier(token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

// Parse a list of comma-separated identifiers in parens or brackets
fn parse_idents_until(delim: Token, lexer: &mut Lexer) -> Result<Vec<String>, ParserError> {
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

fn tokens_until_semicolon(lexer: &mut Lexer) -> Result<Vec<Token>, ParserError> {
	let mut tokens: Vec<Token> = Vec::new();
	while let Some(token) = lexer.consume() {
		if token == Token::Semicolon { return Ok(tokens); }
		tokens.push(token);
	}
	Err(ParserError::UnexpectedEof)
}

pub fn parse(input: &str) -> Result<Vec<Node>, ParserError> {
	let mut lexer = Lexer::new(input);
	let mut nodes: Vec<Node> = Vec::new();

	while let Some(token) = lexer.consume() {
		match token {
			Token::WordConst => {
				let name = parse_identifier(&mut lexer)?;
				expect_token(Token::Equals, &mut lexer)?;
				let tokens = tokens_until_semicolon(&mut lexer)?;
				let value = parse_expr(&tokens)?;
				nodes.push(Node::ConstDef { name, value });
			},

			Token::WordFn => {
				let name = parse_identifier(&mut lexer)?;

				// Parse parameters
				let mut params: Vec<Vec<String>> = Vec::new();
				while let Some(Token::ParenL) = lexer.consume() {
					let inner_params = parse_idents_until(Token::ParenR, &mut lexer)?;
					params.push(inner_params);
				}

				// Type signature
				let type_sig = match lexer.cur {
					Some(Token::Colon) => parse_fn_type(&mut lexer)?,
					Some(Token::Equals | Token::BraceL) => Node::Inferred,
					tok => return handle_bad_token(tok, vec![
						Token::Colon, Token::Equals, Token::BraceL
					]),
				};

				// Function body
				let body = match lexer.cur {
					Some(Token::Equals) => {
						let tokens = tokens_until_semicolon(&mut lexer)?;
						let expr = parse_expr(&tokens)?;
						vec![Node::Return(expr)]
					},
					Some(Token::BraceL) => {
						parse_fn_body(&mut lexer)?
					},
					tok => return handle_bad_token(tok, vec![Token::Equals, Token::BraceL]),
				};

				nodes.push(Node::FnDef { name, params, type_sig, body });
			},

			Token::WordType => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::Equals, &mut lexer)?;
				let tokens = tokens_until_semicolon(&mut lexer)?;
				let value = parse_type_expr(&tokens)?;
				nodes.push(Node::TypeDef { name, params, value });
			},

			Token::WordUnion => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::BraceL, &mut lexer)?;
				let variants = parse_variant_list(&mut lexer)?;
				nodes.push(Node::UnionDef { name, params, variants });
			},

			Token::WordRecord => {
				let name = parse_identifier(&mut lexer)?;
				let params = parse_type_params(&mut lexer)?;
				expect_token(Token::BraceL, &mut lexer)?;
				let fields = parse_field_list(&mut lexer)?;
				nodes.push(Node::RecordDef { name, params, fields });
			},

			Token::Unknown(c) => return Err(ParserError::UnknownCharacter(c)),

			tok => return Err(ParserError::Expected(vec![
				Token::WordConst, Token::WordFn, Token::WordType,
				Token::WordUnion, Token::WordRecord
			], tok)),
		}
	}

	Ok(nodes)
}

use crate::lexer::{ lexer::Lexer, token::Token };
use crate::parser::astnode::Node;

pub enum ParserError {
	UnknownCharacter(char),
	Expected(Vec<Token>, Token),
	UnexpectedEof,
}

fn expect_token(expected: Token, lexer: &mut Lexer) -> Result<(), ParserError> {
	match lexer.next_token() {
		Some(token) if token == expected => Ok(()),
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::Expected(vec![expected], token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

fn expect_oneof(expected: Vec<Token>, lexer: &mut Lexer) -> Result<Token, ParserError> {
	match lexer.next_token() {
		Some(token) if expected.contains(&token) => Ok(token),
		Some(Token::Unknown(c)) => Err(ParserError::UnknownCharacter(c)),
		Some(token) => Err(ParserError::Expected(expected, token)),
		None => Err(ParserError::UnexpectedEof),
	}
}

pub fn parse(input: &str) -> Result<Vec<Node>, ParserError> {
	let mut lexer = Lexer::new(input);
	let mut nodes: Vec<Node> = Vec::new();

	while let Some(token) = lexer.next_token() {
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
				let params = parse_fn_params(&mut lexer)?;

				let tok = expect_oneof(vec![
					Token::Equals, Token::BraceL, Token::Colon
				], &mut lexer)?;

				let type_sig = if tok == Token::Colon {
					parse_fn_type(&mut lexer)?
				} else {
					Node::Inferred
				};

				let body = if tok == Token::Equals {
					let tokens = tokens_until_semicolon(&mut lexer)?;
					let expr = parse_expr(&tokens)?;
					vec![Node::Return(expr)]
				} else {
					parse_fn_body(&mut lexer)?
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

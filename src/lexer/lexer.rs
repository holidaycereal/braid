use crate::lexer::token::Token;

pub struct Lexer {
	chars: Vec<char>,
	pos: usize,
	pub prev: Option<Token>,
	pub cur: Option<Token>,
}

impl Lexer {
	pub fn new(input: &str) -> Self {
		Lexer {
			chars: input.chars().collect(),
			pos: 0,
			prev: None,
			cur: None,
		}
	}

	fn peek(&self, offset: usize) -> Option<char> {
		let peek_pos = self.pos + offset;
		if peek_pos < self.chars.len() { Some(self.chars[peek_pos]) }
		else { None }
	}

	fn current(&self) -> Option<char> {
		self.peek(0)
	}

	fn advance(&mut self) -> Option<char> {
		self.pos += 1;
		let current = self.current()?;
		Some(current)
	}

	fn read_identifier(&mut self) -> String {
		let start = self.pos;
		while let Some(c) = self.advance() {
			if !c.is_alphanumeric() && c != '_' { break; }
		}
		self.chars[start..self.pos].iter().collect()
	}

	fn read_number(&mut self) -> String {
		let start = self.pos;
		let cur = self.current().unwrap();
		let next = self.peek(1);

		// Check for 0x, 0o, or 0b prefix
		let base = match (cur, next) {
			('0', Some('x')) | ('0', Some('X')) => { self.advance(); 16 },
			('0', Some('o')) | ('0', Some('O')) => { self.advance(); 8 },
			('0', Some('b')) | ('0', Some('B')) => { self.advance(); 2 },
			_ => 10,
		};
		// Read integral part
		while let Some(c) = self.advance() {
			if !c.is_digit(base) { break; }
		}

		if base == 10 {
			// Fractional part
			if let Some('.') = self.current() {
				while let Some(c) = self.advance() {
					if !c.is_digit(10) { break; }
				}
			}
			// Scientific notation
			if let Some('e') | Some('E') = self.current() {
				if let Some('+' | '-') = self.advance() {
					self.advance();
				}
				while let Some(c) = self.current() {
					if !c.is_digit(10) { break; }
					self.advance();
				}
			}
		}

		self.chars[start..self.pos].iter().collect()
	}

	fn read_textual_literal(&mut self) -> String {
		let start = self.pos + 1;  // Skip opening quote
		let is_string = self.current().unwrap() == '"';

		while let Some(c) = self.advance() {
			if c == '\\' {
				self.advance();  // Skip escaped character
			} else if (c == '"' && is_string) || (c == '\'' && !is_string) {
				self.advance();
				break;
			}
		}

		let end = self.pos - 1;  // Skip closing quote
		if start + 1 < end { self.chars[start..end].iter().collect() }
		else { String::new() }
	}

	fn skip_whitespace(&mut self) {
		while let Some(c) = self.current() {
			if !c.is_whitespace() { break; }
			self.advance();
		}
	}

	fn lex_token(&mut self) -> Option<Token> {
		self.skip_whitespace();
		let current = self.current()?;

		// Identifiers and keywords
		if current.is_alphabetic() || current == '_' {
			let word = self.read_identifier();
			match word.as_str() {
				// Check for keyword {{{
				"true" => Some(Token::WordTrue),
				"false" => Some(Token::WordFalse),
				"let" => Some(Token::WordLet),
				"return" => Some(Token::WordReturn),
				"if" => Some(Token::WordIf),
				"then" => Some(Token::WordThen),
				"else" => Some(Token::WordElse),
				"elif" => Some(Token::WordElif),
				"case" => Some(Token::WordCase),
				"of" => Some(Token::WordOf),
				"end" => Some(Token::WordEnd),
				"while" => Some(Token::WordWhile),
				"for" => Some(Token::WordFor),
				"in" => Some(Token::WordIn),
				"do" => Some(Token::WordDo),
				"done" => Some(Token::WordDone),
				"break" => Some(Token::WordBreak),
				"continue" => Some(Token::WordContinue),
				"match" => Some(Token::WordMatch),
				"when" => Some(Token::WordWhen),
				"const" => Some(Token::WordConst),
				"type" => Some(Token::WordType),
				"fn" => Some(Token::WordFn),
				"record" => Some(Token::WordRecord),
				"union" => Some(Token::WordUnion),
				"trait" => Some(Token::WordTrait),
				"impl" => Some(Token::WordImpl),
				"and" => Some(Token::WordAnd),
				"or" => Some(Token::WordOr),
				"xor" => Some(Token::WordXor),
				"not" => Some(Token::WordNot),
				// }}}
				_ => Some(Token::Identifier(word)),
			}
		}

		// Numbers
		else if current.is_digit(10) {
			let num_str = self.read_number();

			// Determine the numeric base (16, 8, 2, or 10)
			let (base, num_body) = if num_str.len() > 1 {
				match &num_str[..2] {
					"0x" | "0X" => (16, &num_str[2..]),
					"0o" | "0O" => (8, &num_str[2..]),
					"0b" | "0B" => (2, &num_str[2..]),
					_ => (10, &num_str[..]),
				}
			} else {
				(10, &num_str[..])
			};

			// Make float or int literal token
			if num_str.contains('.')
			|| num_str.contains('e')
			|| num_str.contains('E') {
				Some(Token::FloatLiteral(num_str))
			} else if let Ok(int_value) = u64::from_str_radix(num_body, base) {
				Some(Token::IntLiteral(int_value))
			} else {
				panic!(
					"Integer literal badly formatted or out of range: {}",
					num_str
				);
			}
		}

		// String and character literals
		else if current == '"' {
			Some(Token::StringLiteral(self.read_textual_literal()))
		} else if current == '\'' {
			Some(Token::CharLiteral(self.read_textual_literal()))
		}

		// Symbols and comments
		else {
			let next = self.peek(1);

			match (current, next) {
				// Skip line comments
				('-', Some('-')) => {
					self.advance();
					while let Some(c) = self.advance() {
						if c == '\n' { break; }
					}
					self.lex_token()
				},

				// Skip block comments
				('-', Some('*')) => {
					self.advance();
					self.advance();
					let mut depth = 1;
					while let (Some(cur), Some(next)) =
						(self.current(), self.peek(1))
					{
						self.advance();
						match (cur, next) {
							('-', '*') => {
								self.advance();
								depth += 1;
							},
							('*', '-') => {
								self.advance();
								depth -= 1;
								if depth == 0 { break; }
							},
							_ => {},
						}
					}
					self.lex_token()
				},

				_ => {
					// Handle two- and one-character symbols
					let (symbol, is_long) = match (current, next) {
						// {{{
						('-', Some('>')) => (Some(Token::Arrow), true),
						('=', Some('>')) => (Some(Token::FatArrow), true),
						('|', Some('|')) => (Some(Token::DoublePipe), true),
						('=', Some('=')) => (Some(Token::TestEq), true),
						('!', Some('=')) => (Some(Token::TestNe), true),
						('<', Some('=')) => (Some(Token::CompLe), true),
						('>', Some('=')) => (Some(Token::CompGe), true),
						('>', Some('>')) => (Some(Token::FwdCompose), true),
						('.', Some('.')) => (Some(Token::ExclusiveRange), true),
						('.', Some('*')) => (Some(Token::InclusiveRange), true),
						(':', Some(':')) => (Some(Token::ModuleAccess), true),
						('+', Some('+')) => (Some(Token::Concat), true),
						('+', Some('=')) => (Some(Token::AddAssign), true),
						('-', Some('=')) => (Some(Token::SubAssign), true),
						('*', Some('=')) => (Some(Token::MulAssign), true),
						('/', Some('=')) => (Some(Token::DivAssign), true),
						('%', Some('=')) => (Some(Token::ModAssign), true),
						('|', Some('=')) => (Some(Token::InfixAssign), true),
						('(', _) => (Some(Token::ParenL), false),
						(')', _) => (Some(Token::ParenR), false),
						('[', _) => (Some(Token::BracketL), false),
						(']', _) => (Some(Token::BracketR), false),
						('{', _) => (Some(Token::BraceL), false),
						('}', _) => (Some(Token::BraceR), false),
						('.', _) => (Some(Token::Dot), false),
						(',', _) => (Some(Token::Comma), false),
						(';', _) => (Some(Token::Semicolon), false),
						(':', _) => (Some(Token::Colon), false),
						('=', _) => (Some(Token::Equals), false),
						('|', _) => (Some(Token::Pipe), false),
						('!', _) => (Some(Token::Bang), false),
						('&', _) => (Some(Token::Ampersand), false),
						('#', _) => (Some(Token::Hash), false),
						('-', _) => (Some(Token::Minus), false),
						('+', _) => (Some(Token::Plus), false),
						('*', _) => (Some(Token::Star), false),
						('/', _) => (Some(Token::Slash), false),
						('%', _) => (Some(Token::Percent), false),
						('<', _) => (Some(Token::Less), false),
						('>', _) => (Some(Token::Greater), false),
						// }}}
						_ => (None, false),
					};
					match (symbol, is_long) {
						(Some(sym), true) => {
							self.advance();
							self.advance();
							Some(sym)
						},
						(Some(sym), false) => {
							self.advance();
							Some(sym)
						},
						_ => {
							let c = self.current().unwrap_or('\0');
							self.advance();
							Some(Token::Unknown(c))
						},
					}
				},
			}
		}
	}

	pub fn consume(&mut self) -> Option<Token> {
		self.prev = self.cur.clone();
		self.cur = self.lex_token();
		self.cur.clone()
	}
}

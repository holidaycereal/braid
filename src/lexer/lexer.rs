use crate::lexer::token::Token;

pub struct Lexer {
	chars: Vec<char>,
	pos: usize,
}

impl<'a> Lexer {
	pub fn new(input: &'a str) -> Self {
		let chars: Vec<char> = input.chars().collect();
		Lexer { chars, pos: 0 }
	}

	fn peek(&self, offset: usize) -> Option<char> {
		let peek_pos = self.pos + offset;
		if peek_pos < self.chars.len() {
			Some(self.chars[peek_pos])
		} else {
			None
		}
	}

	fn current(&self) -> Option<char> {
		self.peek(0)
	}

	fn advance(&mut self) -> Option<char> {
		let current = self.current()?;
		self.pos += 1;
		Some(current)
	}

	fn read_identifier(&mut self) -> String {
		let start = self.pos;
		self.advance();

		while let Some(c) = self.current() {
			if c.is_alphanumeric() || c == '_' {
				self.advance();
			} else {
				break;
			}
		}

		self.chars[start..self.pos].iter().collect()
	}

	fn read_number(&mut self) -> String {
		let start = self.pos;

		// Check for 0x, 0o, or 0b prefix
		if let Some('0') = self.current() {
			if let Some(next) = self.peek(1) {
				match next {
					// Hexadecimal
					'x' | 'X' => {
						self.advance();
						self.advance();
						while let Some(c) = self.current() {
							if c.is_digit(16) {
								self.advance();
							} else {
								break;
							}
						}
					},

					// Octal
					'o' | 'O' => {
						self.advance();
						self.advance();
						while let Some(c) = self.current() {
							if c.is_digit(8) {
								self.advance();
							} else {
								break;
							}
						}
					},

					// Binary
					'b' | 'B' => {
						self.advance();
						self.advance();
						while let Some(c) = self.current() {
							if c == '0' || c == '1' {
								self.advance();
							} else {
								break;
							}
						}
					},

					// Decimal - fall through
					_ => {},
				}
			}
		}

		// Decimal
		// Integral part
		while let Some(c) = self.current() {
			if c.is_digit(10) {
				self.advance();
			} else {
				break;
			}
		}

		// Fractional part
		if let Some('.') = self.current() {
			self.advance();
			while let Some(c) = self.current() {
				if c.is_digit(10) {
					self.advance();
				} else {
					break;
				}
			}
		}

		// Scientific notation
		if let Some('e') | Some('E') = self.current() {
			self.advance();
			if let Some('+' | '-') = self.current() {
				self.advance();
			}
			while let Some(c) = self.current() {
				if c.is_digit(10) {
					self.advance();
				} else {
					break;
				}
			}
		}

		self.chars[start..self.pos].iter().collect()
	}

	fn read_textual_literal(&mut self, is_string: bool) -> String {
		let start = self.pos;

		while let Some(c) = self.current() {
			if c == '\\' {
				self.advance();
				if self.current().is_some() {
					self.advance(); // Skip the escaped character
				}
			} else if (c == '"' && is_string) || (c == '\'' && !is_string) {
				self.advance();
				break;
			} else {
				self.advance();
			}
		}

		// Extract the string without the quotes
		let end = self.pos - 1;
		if start + 1 < end {
			self.chars[start..end].iter().collect()
		} else {
			String::new()
		}
	}

	fn skip_whitespace(&mut self) {
		while let Some(c) = self.current() {
			if c.is_whitespace() {
				self.advance();
			} else {
				break;
			}
		}
	}

	pub fn next_token(&mut self) -> Token {
		self.skip_whitespace();

		let current = match self.current() {
			None => return Token::EofToken,
			Some(c) => c,
		};

		// Identifiers and keywords
		if current.is_alphabetic() || current == '_' {
			let word = self.read_identifier();
			match word.as_str() {
				// Check for keyword {{{
				"true" => Token::WordTrue,
				"false" => Token::WordFalse,
				"let" => Token::WordLet,
				"return" => Token::WordReturn,
				"if" => Token::WordIf,
				"then" => Token::WordThen,
				"else" => Token::WordElse,
				"elif" => Token::WordElif,
				"case" => Token::WordCase,
				"of" => Token::WordOf,
				"end" => Token::WordEnd,
				"while" => Token::WordWhile,
				"for" => Token::WordFor,
				"in" => Token::WordIn,
				"do" => Token::WordDo,
				"done" => Token::WordDone,
				"break" => Token::WordBreak,
				"continue" => Token::WordContinue,
				"match" => Token::WordMatch,
				"when" => Token::WordWhen,
				"const" => Token::WordConst,
				"type" => Token::WordType,
				"fn" => Token::WordFn,
				"record" => Token::WordRecord,
				"union" => Token::WordUnion,
				"trait" => Token::WordTrait,
				"impl" => Token::WordImpl,
				"and" => Token::WordAnd,
				"or" => Token::WordOr,
				"xor" => Token::WordXor,
				"not" => Token::WordNot,
				// }}}
				_ => Token::Identifier(word),
			}
		}

		// Numbers
		else if current.is_digit(10) {
			let num_str = self.read_number();

			// Determine the numeric base (16, 8, 2, or 10)
			let (base, num_body) =
				if num_str.starts_with("0x") || num_str.starts_with("0X") {
					(16, &num_str[2..])
				} else if num_str.starts_with("0o") || num_str.starts_with("0O") {
					(8, &num_str[2..])
				} else if num_str.starts_with("0b") || num_str.starts_with("0B") {
					(2, &num_str[2..])
				} else {
					(10, &num_str[..])
				};

			// Make float or int literal token
			if num_str.contains('.') || num_str.contains('e') || num_str.contains('E') {
				Token::FloatLiteral(num_str)
			} else if let Ok(int_value) = u64::from_str_radix(num_body, base) {
				Token::IntLiteral(int_value)
			} else {
				panic!("Integer literal badly formatted or out of range: {}", num_str);
			}
		}

		// String and character literals
		else if current == '"' {
			self.advance();
			Token::StringLiteral(self.read_textual_literal(true))
		} else if current == '\'' {
			self.advance();
			Token::CharLiteral(self.read_textual_literal(false))
		}

		// Symbols and comments
		else {
			let next = self.peek(1);

			match (current, next) {
				// Skip line comments
				('-', Some('-')) => {
					self.advance();
					self.advance();
					while let Some(c) = self.current() {
						if c == '\n' {
							break;
						}
						self.advance();
					}
					self.next_token()
				},

				// Skip block comments
				('-', Some('*')) => {
					self.advance();
					self.advance();
					let mut depth = 1;
					while depth > 0 && self.current().is_some() {
						if let (Some(cur), Some(next)) = (self.current(), self.peek(1)) {
							match (cur, next) {
								('-', '*') => {
									self.advance();
									self.advance();
									depth += 1;
								},
								('*', '-') => {
									self.advance();
									self.advance();
									depth -= 1;
								},
								_ => {
									self.advance();
								},
							}
						} else {
							self.advance();
						}
					}
					self.next_token()
				},

				_ => {
					// Handle two- and one-character symbols
					let (symbol, is_long) = match (current, next) {
						// {{{
						('-', Some('>')) => (Some(Token::Arrow), true),
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
							sym
						},
						(Some(sym), false) => {
							self.advance();
							sym
						},
						(None, _) => {
							let c = self.advance().unwrap_or('\0');
							Token::Unknown(c)
						},
					}
				},
			}
		}
	}
}

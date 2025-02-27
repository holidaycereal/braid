use crate::lexer::token::Token;

pub struct Lexer<'a> {
	input: &'a [u8],
	pos: usize,
}

impl<'a> Lexer<'a> {
	pub fn new(input: &'a str) -> Self {
		Lexer { input: input.as_bytes(), pos: 0 }
	}

	fn peek(&self, offset: usize) -> Option<u8> {
		let peek_pos = self.pos + offset;
		if peek_pos < self.input.len() {
			Some(self.input[peek_pos])
		} else {
			None
		}
	}

	fn current(&self) -> Option<u8> {
		self.peek(0)
	}

	fn advance(&mut self) -> Option<u8> {
		let current = self.current()?;
		self.pos += 1;
		Some(current)
	}

	fn read_identifier(&mut self) -> String {
		let start = self.pos;
		self.advance();

		while let Some(c) = self.current() {
			if c.is_ascii_alphanumeric() || c == b'_' {
				self.advance();
			} else {
				break;
			}
		}

		String::from_utf8_lossy(&self.input[start..self.pos]).to_string()
	}

	fn read_number(&mut self) -> String {
		let start = self.pos;

		// Check for 0x, 0o, or 0b prefix
		if let Some(b'0') = self.current() {
			if let Some(next) = self.peek(1) {
				match next {

					// Hexadecimal
					b'x' | b'X' => {
						self.advance();
						self.advance();
						while let Some(c) = self.current() {
							if c.is_ascii_hexdigit() {
								self.advance();
							} else {
								break;
							}
						}
					},

					// Octal
					b'o' | b'O' => {
						self.advance();
						self.advance();
						while let Some(c) = self.current() {
							if c.is_ascii_digit() && c < b'8' {
								self.advance();
							} else {
								break;
							}
						}
					},

					// Binary
					b'b' | b'B' => {
						self.advance();
						self.advance();
						while let Some(c) = self.current() {
							if c == b'0' || c == b'1' {
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
			if c.is_ascii_digit() {
				self.advance();
			} else {
				break;
			}
		}

		// Fractional part
		if let Some(b'.') = self.current() {
			self.advance();
			while let Some(c) = self.current() {
				if c.is_ascii_digit() {
					self.advance();
				} else {
					break;
				}
			}
		}

		// Scientific notation
		if let Some(b'e') | Some(b'E') = self.current() {
			self.advance();
			if let Some(b'+' | b'-') = self.current() {
				self.advance();
			}
			while let Some(c) = self.current() {
				if c.is_ascii_digit() {
					self.advance();
				} else {
					break;
				}
			}
		}

		String::from_utf8_lossy(&self.input[start..self.pos]).to_string()
	}

	fn read_textual_literal(&mut self, is_string: bool) -> String {
		let start = self.pos;

		while let Some(c) = self.current() {
			if c == b'\\' {
				self.advance();
			} else if c == b'"' && is_string || c == b'\'' && !is_string {
				self.advance();
				break;
			}
			self.advance();
		}

		String::from_utf8_lossy(&self.input[start..self.pos - 1]).to_string()
	}

	fn skip_whitespace(&mut self) {
		while let Some(c) = self.current() {
			if c.is_ascii_whitespace() {
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
		if current.is_ascii_alphabetic() || current == b'_' {
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
		else if current.is_ascii_digit() {
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
		else if current == b'"' {
			self.advance();
			Token::StringLiteral(self.read_textual_literal(true))
		} else if current == b'\'' {
			self.advance();
			Token::CharLiteral(self.read_textual_literal(false))
		}

		// Symbols and comments
		else if let Some(next) = self.peek(1) {
			let symbol = match (current, next) {

				// Skip line comments
				(b'-', b'-') => {
					self.advance();
					self.advance();
					while let Some(c) = self.current() {
						if c == b'\n' {
							break;
						}
						self.advance();
					}
					self.next_token()
				},

				// Skip block comments
				(b'-', b'*') => {
					self.advance();
					self.advance();
					let mut depth = 1;
					while let Some(c) = self.current() {
						if c == b'-' {
							self.advance();
							if let Some(b'*') = self.current() {
								depth += 1;
							}
						} else if c == b'*' {
							self.advance();
							if let Some(b'-') = self.current() {
								depth -= 1;
								if depth == 0 {
									self.advance();
									break;
								}
							}
						}
						self.advance();
					}
					self.next_token()
				},

				// Symbols {{{
				(b'-', b'>') => Token::Arrow,
				(b'=', b'=') => Token::TestEq,
				(b'!', b'=') => Token::TestNe,
				(b'<', b'=') => Token::CompLe,
				(b'>', b'=') => Token::CompGe,
				(b'>', b'>') => Token::FwdCompose,
				(b'.', b'.') => Token::ExclusiveRange,
				(b'.', b'*') => Token::InclusiveRange,
				(b':', b':') => Token::ModuleAccess,
				(b'+', b'+') => Token::Concat,
				(b'+', b'=') => Token::AddAssign,
				(b'-', b'=') => Token::SubAssign,
				(b'*', b'=') => Token::MulAssign,
				(b'/', b'=') => Token::DivAssign,
				(b'%', b'=') => Token::ModAssign,
				(b'(', _) => Token::ParenL,
				(b')', _) => Token::ParenR,
				(b'[', _) => Token::BracketL,
				(b']', _) => Token::BracketR,
				(b'{', _) => Token::BraceL,
				(b'}', _) => Token::BraceR,
				(b'.', _) => Token::Dot,
				(b',', _) => Token::Comma,
				(b';', _) => Token::Semicolon,
				(b':', _) => Token::Colon,
				(b'=', _) => Token::Equals,
				(b'|', _) => Token::Pipe,
				(b'!', _) => Token::Bang,
				(b'&', _) => Token::Ampersand,
				(b'#', _) => Token::Hash,
				(b'-', _) => Token::Minus,
				(b'+', _) => Token::Plus,
				(b'*', _) => Token::Star,
				(b'/', _) => Token::Slash,
				(b'%', _) => Token::Percent,
				(b'<', _) => Token::Less,
				(b'>', _) => Token::Greater,
				// }}}
				_ => Token::Unknown(current),
			};

			match symbol {
				Token::Unknown(c) => {
					panic!("Unknown character: {}", c);
				},
				// Handle two- and one-character symbols
				Token::Arrow | Token::TestEq | Token::TestNe | Token::CompLe
				| Token::CompGe | Token::FwdCompose | Token::ExclusiveRange
				| Token::InclusiveRange | Token::ModuleAccess | Token::Concat
				| Token::AddAssign | Token::SubAssign | Token::MulAssign
				| Token::DivAssign | Token::ModAssign => {
					self.advance();
					self.advance();
					symbol
				},
				_ => {
					self.advance();
					symbol
				},
			}
		}

		else {
			Token::EofToken
		}
	}
}

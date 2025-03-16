use crate::lexer::token::Token;
use crate::parser::parser::ParserError;

pub struct Lexer {
	chars: Vec<char>,
	pos: usize,
	pub current: Option<Token>,
}

impl Lexer {
	pub fn new(input: &str) -> Self {
		Lexer {
			chars: input.chars().collect(),
			pos: 0,
			current: None,
		}
	}

	pub fn consume(&mut self) -> Option<Token> {
		self.current = self.lex_token();
		self.current.clone()
	}

	// Step backwards until we find a semicolon, `end` or `else`
	pub fn goto_last_terminator(&mut self) -> Result<(), ParserError> {
		while self.pos > 0 {
			match self.peek(0).unwrap() {
				// Found semicolon terminator
				';' => return Ok(()),

				// Check if this is an `end` or `else`
				'e' => {
					let prev = self.peek(-1).unwrap();
					// Continue if the e is in the middle of the word
					if prev.is_alphanumeric() || prev == '_' { continue; }
					// Record start position so we can go back to it
					let start = self.pos;
					match self.read_identifier().as_str() {
						"end" | "else" => return Ok(()),
						_ => { self.pos = start; },
					}
				},

				_ => {},
			}
			// Unconditionally step backwards
			self.pos -= 1;
		}
		Err(ParserError::LastTerminatorNotFound)
		// I think this error is unreachable... I think
	}

	fn peek(&self, offset: isize) -> Option<char> {
		let peek_pos = (self.pos as isize + offset) as usize;
		if peek_pos < self.chars.len() { Some(self.chars[peek_pos]) }
		else { None }
	}

	fn advance(&mut self) -> Option<char> {
		self.pos += 1;
		self.peek(0)
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
		let cur = self.peek(0).unwrap();
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
			if let Some('.') = self.peek(0) {
				while let Some(c) = self.advance() {
					if !c.is_digit(10) { break; }
				}
			}
			// Scientific notation
			if let Some('e' | 'E') = self.peek(0) {
				if let Some('+' | '-') = self.advance() {
					self.advance();
				}
				while let Some(c) = self.peek(0) {
					if !c.is_digit(10) { break; }
					self.advance();
				}
			}
		}

		self.chars[start..self.pos].iter().collect()
	}

	fn read_textual_literal(&mut self) -> String {
		let start = self.pos + 1;  // Skip opening quote
		let is_string = self.peek(0).unwrap() == '"';

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
		while let Some(c) = self.peek(0) {
			if !c.is_whitespace() { break; }
			self.advance();
		}
	}

	fn lex_token(&mut self) -> Option<Token> {
		self.skip_whitespace();
		let current = self.peek(0)?;

		// Identifiers and keywords
		if current.is_alphabetic() || current == '_' {
			let word = self.read_identifier();
			match word.as_str() {
				// Check for keyword {{{
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
				"where" => Some(Token::WordWhere),
				"do" => Some(Token::WordDo),
				"done" => Some(Token::WordDone),
				"break" => Some(Token::WordBreak),
				"continue" => Some(Token::WordContinue),
				"match" => Some(Token::WordMatch),
				"test" => Some(Token::WordTest),
				"const" => Some(Token::WordConst),
				"type" => Some(Token::WordType),
				"def" => Some(Token::WordDef),
				"proc" => Some(Token::WordProc),
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
					while let (Some(cur), Some(nxt)) = (self.peek(0), self.peek(1)) {
						self.advance();
						match (cur, nxt) {
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
					let (symbol, length) = match (current, next, self.peek(2)) {
						// {{{
						('.', Some('.'), Some('=')) => (Some(Token::InclusiveRange), 3),
						('+', Some('+'), Some('=')) => (Some(Token::ConcatAssign), 3),
						('.', Some('.'), _) => (Some(Token::ExclusiveRange), 2),
						('+', Some('+'), _) => (Some(Token::Concat), 2),
						('-', Some('>'), _) => (Some(Token::Arrow), 2),
						('=', Some('>'), _) => (Some(Token::FatArrow), 2),
						('|', Some('|'), _) => (Some(Token::DoublePipe), 2),
						('=', Some('='), _) => (Some(Token::TestEq), 2),
						('!', Some('='), _) => (Some(Token::TestNe), 2),
						('<', Some('='), _) => (Some(Token::CompLe), 2),
						('>', Some('='), _) => (Some(Token::CompGe), 2),
						('>', Some('>'), _) => (Some(Token::FwdCompose), 2),
						(':', Some(':'), _) => (Some(Token::ModuleAccess), 2),
						('+', Some('='), _) => (Some(Token::AddAssign), 2),
						('-', Some('='), _) => (Some(Token::SubAssign), 2),
						('*', Some('='), _) => (Some(Token::MulAssign), 2),
						('/', Some('='), _) => (Some(Token::DivAssign), 2),
						('%', Some('='), _) => (Some(Token::ModAssign), 2),
						('|', Some('='), _) => (Some(Token::InfixAssign), 2),
						('(', _, _) => (Some(Token::ParenL), 1),
						(')', _, _) => (Some(Token::ParenR), 1),
						('[', _, _) => (Some(Token::BracketL), 1),
						(']', _, _) => (Some(Token::BracketR), 1),
						('{', _, _) => (Some(Token::BraceL), 1),
						('}', _, _) => (Some(Token::BraceR), 1),
						('.', _, _) => (Some(Token::Dot), 1),
						(',', _, _) => (Some(Token::Comma), 1),
						(';', _, _) => (Some(Token::Semicolon), 1),
						(':', _, _) => (Some(Token::Colon), 1),
						('=', _, _) => (Some(Token::Equals), 1),
						('|', _, _) => (Some(Token::Pipe), 1),
						('!', _, _) => (Some(Token::Bang), 1),
						('&', _, _) => (Some(Token::Ampersand), 1),
						('#', _, _) => (Some(Token::Hash), 1),
						('^', _, _) => (Some(Token::Caret), 1),
						('~', _, _) => (Some(Token::Tilde), 1),
						('-', _, _) => (Some(Token::Minus), 1),
						('+', _, _) => (Some(Token::Plus), 1),
						('*', _, _) => (Some(Token::Star), 1),
						('/', _, _) => (Some(Token::Slash), 1),
						('%', _, _) => (Some(Token::Percent), 1),
						('<', _, _) => (Some(Token::Less), 1),
						('>', _, _) => (Some(Token::Greater), 1),
						// }}}
						_ => (None, 0),
					};

					for _ in 0..length { self.advance(); }

					match symbol {
						Some(sym) => Some(sym),
						_ => {
							let c = self.peek(0).unwrap_or('\0');
							self.advance();
							Some(Token::Unknown(c))
						},
					}
				},
			}
		}
	}
}

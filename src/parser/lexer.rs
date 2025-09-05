use std::io::{ self, Read, BufReader };
use std::fs::File;
use std::str;
use crate::parser::token::{ Token, get_keyword_map, SYMBOL_DEFS };
use crate::parser::syntax_error::{ SyntaxError, SyntaxErrorKind };

type LexerResult = io::Result<Result<Option<Token>, SyntaxError>>;

pub struct Lexer {
    reader: BufReader<File>,
    current: Option<char>,
    peek1: Option<char>,
    peek2: Option<char>,
    pub line: usize,
    pub column: usize,
}

impl Lexer {
    pub fn new(file: File) -> io::Result<Self> {
        let mut lexer = Self {
            reader: BufReader::new(file),
            current: None,
            peek1: None,
            peek2: None,
            line: 1,
            column: 0,
        };
        lexer.current = lexer.read_char()?;
        lexer.peek1 = lexer.read_char()?;
        lexer.peek2 = lexer.read_char()?;
        Ok(lexer)
    }

    fn produce_syntax_error(&self, kind: SyntaxErrorKind) -> SyntaxError {
        SyntaxError { kind, line: self.line, column: self.column }
    }

    fn read_char(&mut self) -> io::Result<Option<char>> {
        let mut buf = [0_u8; 4]; // buffer for a UTF-8 character

        // read one byte at a time until we have a valid character
        for i in 0..4 {
            if self.reader.read(&mut buf[i..i+1])? == 0 { // EOF encountered
                if i != 0 { break; }
                return Ok(None);
            }

            // NOTE: change this to `if let Ok(ch) = char::decode_utf8(...)` once
            // `char::decode_utf8` becomes stable
            if let Ok(s) = str::from_utf8(&buf[..i+1]) {
                let c = s.chars().next().unwrap();
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += 1;
                }
                return Ok(Some(c));
            }
        }

        Err(io::Error::new(io::ErrorKind::InvalidData, "invalid UTF-8 character"))
    }

    fn advance(&mut self) -> io::Result<Option<char>> {
        self.current = self.peek1;
        self.peek1 = self.peek2;
        self.peek2 = self.read_char()?;
        Ok(self.current)
    }

    fn advance_while<F: Fn(char) -> bool>(&mut self, predicate: F) -> io::Result<String> {
        let mut acc = String::new();
        while let Some(c) = self.current {
            if !predicate(c) { break; }
            acc.push(c);
            self.advance()?;
        }
        Ok(acc)
    }

    pub fn next_token(&mut self) -> LexerResult {
        // skip whitespace
        while let Some(c) = self.current {
            if c.is_whitespace() {
                self.advance()?;
                continue;
            }
            break;
        }

        // skip commments
        match self.skip_comment()? {
            Ok(true) => return self.next_token(),
            Ok(false) => {},
            Err(err) => return Ok(Err(err)),
        };

        match self.current {
            Some(c) if c.is_alphabetic() || c == '_' => self.read_word(),
            Some(c) if c.is_digit(10)                => self.read_number(),
            Some('"' | '\'')                         => self.read_text_literal(),
            Some(_)                                  => self.read_symbol(),
            None => Ok(Ok(None)),
        }
    }

    // read a keyword or identifier
    fn read_word(&mut self) -> LexerResult {
        let word = self.advance_while(|c| c.is_alphanumeric() || c == '_')?;
        Ok(Ok(Some(get_keyword_map()
            .get(word.as_str())
            .cloned()
            .unwrap_or(Token::Identifier(word))
        )))
    }

    // read a numeric literal
    fn read_number(&mut self) -> LexerResult {
        let mut acc = String::from(self.current.unwrap());

        let radix = match (self.current.unwrap(), self.peek1, self.peek2) {
            ('0', Some('b' | 'B'), Some(c)) if c.is_digit(2) => 2,
            ('0', Some('o' | 'O'), Some(c)) if c.is_digit(8) => 8,
            ('0', Some('x' | 'X'), Some(c)) if c.is_digit(16) => 16,
            _ => 10,
        };

        self.advance()?;

        if radix != 10 {
            acc.push(self.current.unwrap());
            self.advance()?;
            acc.push_str(&self.advance_while(|c| c.is_digit(radix))?);
            return Ok(Ok(Some(Token::Literal(acc))));
        }

        acc.push_str(&self.advance_while(|c| c.is_digit(radix))?);

        // read fractional part
        if let (Some('.'), Some(c)) = (self.current, self.peek1) {
            if c.is_digit(10) {
                acc.push(self.current.unwrap());
                self.advance()?;
                acc.push_str(&self.advance_while(|c| c.is_digit(10))?);
            }
        }

        // read exponent part
        if let Some('e' | 'E') = self.current {
            acc.push(self.current.unwrap());
            self.advance()?;

            if let Some('-' | '+') = self.current {
                acc.push(self.current.unwrap());
                self.advance()?;
            }

            match self.current {
                Some(c) if c.is_digit(10) => acc.push_str(&self.advance_while(|c| c.is_digit(10))?),
                _ => return Ok(Err(self.produce_syntax_error(SyntaxErrorKind::InvalidNumericLiteral))),
            };
        }

        Ok(Ok(Some(Token::Literal(acc))))
    }

    fn read_text_literal(&mut self) -> LexerResult {
        let delim = self.current.unwrap();
        let mut acc = String::from(delim);
        self.advance()?;

        while let Some(c) = self.current {
            if c == delim {
                acc.push(delim);
                self.advance()?;
                return Ok(Ok(Some(Token::Literal(acc))));
            }

            if c == '\\' {
                acc.push('\\');
                self.advance()?;
                if let Some(esc) = self.current {
                    acc.push(esc);
                    self.advance()?;
                } else {
                    break;
                }
            } else {
                acc.push(c);
                self.advance()?;
            }
        }

        Ok(Err(self.produce_syntax_error(SyntaxErrorKind::UnterminatedLiteral)))
    }

    fn read_symbol(&mut self) -> LexerResult {
        let mut lookahead = String::new();
        if let Some(c) = self.current { lookahead.push(c); }
        if let Some(c) = self.peek1   { lookahead.push(c); }
        if let Some(c) = self.peek2   { lookahead.push(c); }

        for len in (1..=3).rev() {
            if lookahead.len() < len { continue; }
            let slice = &lookahead[..len];

            if let Some((_, token)) = SYMBOL_DEFS.iter().find(|(s, _)| *s == slice) {
                for _ in 0..len { self.advance()?; }
                return Ok(Ok(Some(token.clone())));
            }
        }

        let unknown_char = self.current.unwrap();
        self.advance()?;
        Ok(Err(self.produce_syntax_error(SyntaxErrorKind::UnknownCharacter(unknown_char))))
    }

    fn skip_comment(&mut self) -> io::Result<Result<bool, SyntaxError>> {
        match (self.current, self.peek1) {
            // line comment
            (Some('-'), Some('-')) => {
                self.advance()?;
                self.advance()?;
                self.advance_while(|c| c != '\n')?;
                if let Some('\n') = self.current { self.advance()?; }
                Ok(Ok(true))
            },

            // block comment
            (Some('-'), Some('*')) => {
                self.advance()?;
                self.advance()?;

                let mut depth = 1;
                while depth > 0 {
                    match (self.current, self.peek1) {
                        (Some('-'), Some('*')) => { self.advance()?; self.advance()?; depth += 1; },
                        (Some('*'), Some('-')) => { self.advance()?; self.advance()?; depth -= 1; },
                        (Some(_), _) => { self.advance()?; },
                        (None, _) => return Ok(Err(self.produce_syntax_error(SyntaxErrorKind::UnterminatedComment))),
                    };
                }
                Ok(Ok(true))
            },

            // not a comment
            _ => Ok(Ok(false)),
        }
    }
}

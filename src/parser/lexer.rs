use crate::parser::token::{ Token, get_keyword_map, SYMBOL_DEFS };
use crate::parser::syntax_error::SyntaxError;

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    toks: Vec<Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            chars: input.chars().collect(),
            pos: 0,
            toks: Vec::new(),
        }
    }

    pub fn tokenise(&mut self) -> Result<&Vec<Token>, SyntaxError> {
        while let Some(c) = self.peek(0) {
            // skip whitespace & comments
            if c.is_whitespace() {
                self.pos += 1;
                continue;
            }
            self.skip_comments()?;
            // make a token and append it to toks
            let token = if c.is_alphabetic() || c == '_' {
                self.read_word()
            } else if c.is_digit(10) {
                self.read_number()
            } else if c == '"' {
                self.read_text_literal(Token::StringLiteral)?
            } else if c == '\'' {
                self.read_text_literal(Token::CharLiteral)?
            } else {
                self.read_symbol()?
            };
            self.toks.push(token);
        }
        Ok(&self.toks)
    }

    // handles a letter or underscore.
    // reads till the end of the word, tries to match it with a keyword token,
    // and falls back to creating an identifier token
    fn read_word(&mut self) -> Token {
        let word = self.consume_while(|c| c.is_alphanumeric() || c == '_');
        get_keyword_map()
            .get(word.as_str())
            .cloned()
            .unwrap_or_else(|| Token::Identifier(word))
    }

    // reads a numeric literal when we encounter a digit.
    // numeric literals supported:
    // - int literals (binary, octal, hexadecimal, decimal)
    // - float literals with decimal point
    // - exponent literals (scientific notation)
    fn read_number(&mut self) -> Token {
        let base = match (self.current(), self.peek(1), self.peek(2)) {
            ('0', Some('b' | 'B'), Some(d)) if d.is_digit(2)  => 2,
            ('0', Some('o' | 'O'), Some(d)) if d.is_digit(8)  => 8,
            ('0', Some('x' | 'X'), Some(d)) if d.is_digit(16) => 16,
            _ => 10,
        };
        if base != 10 { self.pos += 2; } // skip prefix
        let int = self.consume_while(|c| c.is_digit(base)); // read integral part
        if base != 10 {
            Token::IntLiteral { base, int }
        } else {
            match (self.read_frac_part(), self.read_exp_part()) {
                (Some(frac), Some((has_minus, exp))) => Token::FracExpLiteral { int, frac, has_minus, exp },
                (None,       Some((has_minus, exp))) => Token::ExpLiteral { int, has_minus, exp },
                (Some(frac), None)                   => Token::FracLiteral { int, frac },
                (None,       None)                   => Token::IntLiteral { base: 10, int },
            }
        }
    }

    // helpers for read_number {{{

    // read the digits after the decimal point
    fn read_frac_part(&mut self) -> Option<String> {
        match (self.peek(0), self.peek(1)) {
            (Some('.'), Some(d)) if d.is_digit(10) => {
                self.pos += 1;
                Some(self.consume_while(|c| c.is_digit(10)))
            },
            _ => None,
        }
    }

    // read the digits after the 'e' in scientific notation
    fn read_exp_part(&mut self) -> Option<(bool, String)> {
        match self.peek(0) {
            Some('e' | 'E') => {
                let (advance, has_minus) = match self.peek(1) {
                    Some('-') if self.peek(2).map_or(false, |d| d.is_digit(10)) => Some((2, true)),
                    Some('+') if self.peek(2).map_or(false, |d| d.is_digit(10)) => Some((2, false)),
                    Some(d) if d.is_digit(10) => Some((1, false)),
                    _ => None,
                }?;
                self.pos += advance;
                Some((has_minus, self.consume_while(|c| c.is_digit(10))))
            },
            _ => None,
        }
    }
    // }}}

    // handles a ' or ".
    // creates either a StringLiteral or CharLiteral.
    fn read_text_literal<F>(&mut self, make_token: F) -> Result<Token, SyntaxError>
    where F: Fn(String) -> Token {
        let mut text = String::new();
        let delim = self.consume().unwrap();
        while let Some(c) = self.consume() {
            if c == delim {
                return Ok(make_token(text));
            } else if c == '\\' {
                match self.consume() {
                    Some(esc) => { text.push('\\'); text.push(esc); },
                    None => { break; },
                }
            } else {
                text.push(c);
            }
        }
        Err(SyntaxError::UnterminatedLiteral)
    }

    // match any other kind of token
    // (doesn't start with a letter, underscore, digit, or quote mark)
    fn read_symbol(&mut self) -> Result<Token, SyntaxError> {
        let remaining = &self.chars[self.pos..];
        for (sym, tok) in SYMBOL_DEFS {
            if remaining.len() >= sym.len()
            && remaining[..sym.len()].iter().collect::<String>() == *sym {
                self.pos += sym.len();
                return Ok(tok.clone());
            }
        }
        Err(SyntaxError::UnknownCharacter(self.current()))
    }

    // basic utility helpers {{{
    fn current(&self) -> char {
        self.chars[self.pos]
    }

    fn peek(&self, offset: usize) -> Option<char> {
        if self.pos + offset >= self.chars.len() { None } else {
            Some(self.chars[self.pos + offset])
        }
    }

    fn consume(&mut self) -> Option<char> {
        if self.pos >= self.chars.len() { None } else {
            let ret = self.current();
            self.pos += 1;
            Some(ret)
        }
    }

    fn consume_while<F>(&mut self, predicate: F) -> String
    where F: Fn(char) -> bool {
        let mut acc = String::new();
        while let Some(c) = self.peek(0) {
            if !predicate(c) { break; }
            acc.push(c);
            self.pos += 1;
        }
        acc
    }

    fn skip_comments(&mut self) -> Result<(), SyntaxError> {
        match self.peek(0) {
            Some('#') => match self.peek(1) {
                // skip block comment, allowing nesting
                Some('{') => {
                    self.pos += 2;
                    let mut depth = 1;
                    while let (Some(cur), Some(next)) = (self.peek(0), self.peek(1)) {
                        match (cur, next) {
                            ('}', '#') => { self.pos += 2; depth -= 1; if depth < 1 { return Ok(()); } },
                            ('#', '{') => { self.pos += 2; depth += 1; },
                            _          => { self.pos += 1; }
                        }
                    }
                    Err(SyntaxError::UnterminatedComment)
                },
                // skip line comment
                _ => {
                    self.consume_while(|c| c != '\n');
                    Ok(())
                },
            },
            _ => Ok (()),
        }
    }
    // }}}
}

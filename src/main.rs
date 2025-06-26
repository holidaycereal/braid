mod lexer;
mod parser;
use crate::lexer::{ token::Token, lexer::Lexer };
use crate::parser::parser::SyntaxError;
use std::{ env, fs, path::Path, process };
use colored::Colorize;

fn main() {
	let argv: Vec<String> = env::args().collect();
	if argv.len() != 2 {
		eprintln!("usage: {} <filename>", argv[0]);
		process::exit(1);
	}

	let path = Path::new(&argv[1]);
	match fs::read_to_string(path) {
		Ok(s) => {
            let mut lexer = Lexer::new(&s);
            match lexer.tokenise() {
                Ok(tokens) => {
                    for token in tokens {
                        print!("{} ", token_to_string(&token));
                    }
                    println!();
                },
                Err(e) => {
                    eprintln!("error: {}", parser_error_to_string(&e));
                    process::exit(1);
                },
            }
        },
		Err(e) => {
			eprintln!("error reading file '{}': {}", path.display(), e);
			process::exit(1);
		},
	}
}

fn token_to_string(token: &Token) -> String {
	match token {
		Token::Identifier(s) => format!("{}", s).to_string(),

		Token::StringLiteral(s) => format!("\"{}\"", s).green().to_string(),
		Token::CharLiteral(s) => format!("'{}'", s).green().to_string(),

        Token::IntLiteral { base, int } => match base {
            2 => format!("0b{}", int),
            8 => format!("0o{}", int),
            16 => format!("0x{}", int),
            _ => format!("{}", int),
        }.yellow().to_string(),

        Token::FracLiteral { int, frac } => format!(
            "{}.{}", int, frac
        ).yellow().to_string(),

        Token::ExpLiteral { int, has_minus, exp } => format!(
            "{}e{}{}", int, if *has_minus { '-' } else { '+' }, exp
        ).yellow().to_string(),

        Token::FracExpLiteral { int, frac, has_minus, exp } => format!(
            "{}.{}e{}{}", int, frac, if *has_minus { '-' } else { '+' }, exp
        ).yellow().to_string(),

        Token::WordLet      => "let"     .magenta().to_string(),
        Token::WordVar      => "var"     .magenta().to_string(),
        Token::WordReturn   => "return"  .magenta().to_string(),
        Token::WordIf       => "if"      .magenta().to_string(),
        Token::WordElse     => "else"    .magenta().to_string(),
        Token::WordElif     => "elif"    .magenta().to_string(),
        Token::WordMatch    => "match"   .magenta().to_string(),
        Token::WordWhere    => "where"   .magenta().to_string(),
        Token::WordFn       => "fn"      .magenta().to_string(),
        Token::WordWhile    => "while"   .magenta().to_string(),
        Token::WordFor      => "for"     .magenta().to_string(),
        Token::WordIn       => "in"      .magenta().to_string(),
        Token::WordBreak    => "break"   .magenta().to_string(),
        Token::WordContinue => "continue".magenta().to_string(),
        Token::WordType     => "type"    .magenta().to_string(),
        Token::WordRecord   => "record"  .magenta().to_string(),
        Token::WordUnion    => "union"   .magenta().to_string(),
        Token::WordIface    => "iface"   .magenta().to_string(),
        Token::WordImpl     => "impl"    .magenta().to_string(),

        Token::InclRange     => "..=".to_string(),
        Token::ConcatAssign  => "++=".to_string(),

        Token::ExclRange     => "..".to_string(),
        Token::Concat        => "++".to_string(),
        Token::CompLe        => "<=".to_string(),
        Token::CompGe        => ">=".to_string(),
        Token::TestEq        => "==".to_string(),
        Token::TestNe        => "!=".to_string(),
        Token::Arrow         => "->".to_string(),
        Token::LogicOr       => "||".to_string(),
        Token::LogicAnd      => "&&".to_string(),
        Token::FwdCompose    => ">>".to_string(),
        Token::PathSeparator => "::".to_string(),
        Token::AddAssign     => "+=".to_string(),
        Token::SubAssign     => "-=".to_string(),
        Token::MulAssign     => "*=".to_string(),
        Token::DivAssign     => "/=".to_string(),
        Token::ModAssign     => "%=".to_string(),
        Token::TernaryLeft   => "??".to_string(),
        Token::TernaryRight  => "!!".to_string(),

        Token::ParenL        => "(".to_string(),
        Token::ParenR        => ")".to_string(),
        Token::BracketL      => "[".to_string(),
        Token::BracketR      => "]".to_string(),
        Token::BraceL        => "{".to_string(),
        Token::BraceR        => "}".to_string(),
        Token::Dot           => ".".to_string(),
        Token::Comma         => ",".to_string(),
        Token::Semicolon     => ";".to_string(),
        Token::Colon         => ":".to_string(),
        Token::Equals        => "=".to_string(),
        Token::Minus         => "-".to_string(),
        Token::Plus          => "+".to_string(),
        Token::Star          => "*".to_string(),
        Token::Slash         => "/".to_string(),
        Token::Percent       => "%".to_string(),
        Token::Less          => "<".to_string(),
        Token::Greater       => ">".to_string(),
        Token::Bang          => "!".to_string(),
        Token::Pipe          => "|".to_string(),
        Token::Ampersand     => "&".to_string(),
        Token::Caret         => "^".to_string(),
	}
}

fn parser_error_to_string(error: &SyntaxError) -> String {
    match error {
        SyntaxError::UnknownCharacter(c) => format!("unknown character '{}'", c),
        SyntaxError::UnterminatedLiteral => "unterminated literal".to_string(),
        SyntaxError::UnterminatedComment => "unterminated comment".to_string(),
    }
}

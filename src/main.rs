mod lexer;
mod parser;
use crate::lexer::token::Token;
use std::{ env, fs, path::Path, process };
use colored::Colorize;

fn token_to_string(token: &Token) -> String {
	match token {
		// {{{
		Token::Unknown(c) => format!("(Unknown {})", c).red().to_string(),
		Token::Identifier(s) => format!("{}", s).to_string(),
		Token::StringLiteral(s) => format!("\"{}\"", s).green().to_string(),
		Token::CharLiteral(s) => format!("'{}'", s).green().to_string(),
		Token::IntLiteral(n) => format!("{}", n).yellow().to_string(),
		Token::FloatLiteral(s) => format!("{}", s).yellow().to_string(),
		Token::WordTrue => "true".yellow().to_string(),
		Token::WordFalse => "false".yellow().to_string(),
		Token::WordLet => "let".blue().to_string(),
		Token::WordReturn => "return".blue().to_string(),
		Token::WordIf => "if".blue().to_string(),
		Token::WordThen => "then".blue().to_string(),
		Token::WordElse => "else".blue().to_string(),
		Token::WordElif => "elif".blue().to_string(),
		Token::WordCase => "case".blue().to_string(),
		Token::WordOf => "of".blue().to_string(),
		Token::WordEnd => "end".blue().to_string(),
		Token::WordWhile => "while".blue().to_string(),
		Token::WordFor => "for".blue().to_string(),
		Token::WordIn => "in".blue().to_string(),
		Token::WordDo => "do".blue().to_string(),
		Token::WordDone => "done".blue().to_string(),
		Token::WordBreak => "break".blue().to_string(),
		Token::WordContinue => "continue".blue().to_string(),
		Token::WordMatch => "match".blue().to_string(),
		Token::WordTest => "test".blue().to_string(),
		Token::WordConst => "const".blue().to_string(),
		Token::WordType => "type".blue().to_string(),
		Token::WordDef => "def".blue().to_string(),
		Token::WordFn => "fn".blue().to_string(),
		Token::WordRecord => "record".blue().to_string(),
		Token::WordUnion => "union".blue().to_string(),
		Token::WordTrait => "trait".blue().to_string(),
		Token::WordImpl => "impl".blue().to_string(),
		Token::WordAnd => "and".blue().to_string(),
		Token::WordOr => "or".blue().to_string(),
		Token::WordXor => "xor".blue().to_string(),
		Token::WordNot => "not".blue().to_string(),
		Token::CompLe => "<=".to_string(),
		Token::CompGe => ">=".to_string(),
		Token::TestEq => "==".to_string(),
		Token::TestNe => "!=".to_string(),
		Token::Arrow => "->".to_string(),
		Token::FatArrow => "=>".to_string(),
		Token::DoublePipe => "||".to_string(),
		Token::FwdCompose => ">>".to_string(),
		Token::ExclusiveRange => "..".to_string(),
		Token::InclusiveRange => ".*".to_string(),
		Token::ModuleAccess => "::".to_string(),
		Token::Concat => "++".to_string(),
		Token::AddAssign => "+=".to_string(),
		Token::SubAssign => "-=".to_string(),
		Token::MulAssign => "*=".to_string(),
		Token::DivAssign => "/=".to_string(),
		Token::ModAssign => "%=".to_string(),
		Token::InfixAssign => "|=".to_string(),
		Token::ParenL => "(".to_string(),
		Token::ParenR => ")".to_string(),
		Token::BracketL => "[".to_string(),
		Token::BracketR => "]".to_string(),
		Token::BraceL => "{".to_string(),
		Token::BraceR => "}".to_string(),
		Token::Dot => ".".to_string(),
		Token::Comma => ",".to_string(),
		Token::Semicolon => ";".to_string(),
		Token::Colon => ":".to_string(),
		Token::Equals => "=".to_string(),
		Token::Pipe => "|".to_string(),
		Token::Bang => "!".to_string(),
		Token::Ampersand => "&".to_string(),
		Token::Hash => "#".to_string(),
		Token::Minus => "-".to_string(),
		Token::Plus => "+".to_string(),
		Token::Star => "*".to_string(),
		Token::Slash => "/".to_string(),
		Token::Percent => "%".to_string(),
		Token::Less => "<".to_string(),
		Token::Greater => ">".to_string(),
		// }}}
	}
}

fn print_tokens(source: &str) {
	let mut lexer = lexer::lexer::Lexer::new(source);
	while let Some(token) = lexer.consume() {
		print!("{} ", token_to_string(&token));
	}
	println!("");
}

fn main() {
	let argv: Vec<String> = env::args().collect();
	if argv.len() != 2 {
		eprintln!("Usage: {} <filename>", argv[0]);
		process::exit(1);
	}

	let path = Path::new(&argv[1]);

	match fs::read_to_string(path) {
		Ok(s) => print_tokens(&s),
		Err(e) => {
			eprintln!("Error reading file '{}': {}", path.display(), e);
			process::exit(1);
		},
	}
}

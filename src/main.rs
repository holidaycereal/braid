mod lexer;
mod parser;
use crate::lexer::token::Token;
use std::{ env, fs, path::Path, process };

fn token_to_string(token: &Token) -> String {
	match token {
		// {{{
		Token::EofToken => "EOF".to_string(),
		Token::Unknown(c) => format!("(Unknown {})", c).to_string(),
		Token::Identifier(s) => format!("{}", s).to_string(),
		Token::StringLiteral(s) => format!("\"{}\"", s).to_string(),
		Token::CharLiteral(s) => format!("'{}'", s).to_string(),
		Token::IntLiteral(n) => format!("{}", n).to_string(),
		Token::FloatLiteral(s) => format!("{}", s).to_string(),
		Token::WordTrue => "TRUE".to_string(),
		Token::WordFalse => "FALSE".to_string(),
		Token::WordLet => "LET".to_string(),
		Token::WordReturn => "RETURN".to_string(),
		Token::WordIf => "IF".to_string(),
		Token::WordThen => "THEN".to_string(),
		Token::WordElse => "ELSE".to_string(),
		Token::WordElif => "ELIF".to_string(),
		Token::WordCase => "CASE".to_string(),
		Token::WordOf => "OF".to_string(),
		Token::WordEnd => "END".to_string(),
		Token::WordWhile => "WHILE".to_string(),
		Token::WordFor => "FOR".to_string(),
		Token::WordIn => "IN".to_string(),
		Token::WordDo => "DO".to_string(),
		Token::WordDone => "DONE".to_string(),
		Token::WordBreak => "BREAK".to_string(),
		Token::WordContinue => "CONTINUE".to_string(),
		Token::WordMatch => "MATCH".to_string(),
		Token::WordWhen => "WHEN".to_string(),
		Token::WordConst => "CONST".to_string(),
		Token::WordType => "TYPE".to_string(),
		Token::WordFn => "FN".to_string(),
		Token::WordRecord => "RECORD".to_string(),
		Token::WordUnion => "UNION".to_string(),
		Token::WordTrait => "TRAIT".to_string(),
		Token::WordImpl => "IMPL".to_string(),
		Token::WordAnd => "AND".to_string(),
		Token::WordOr => "OR".to_string(),
		Token::WordXor => "XOR".to_string(),
		Token::WordNot => "NOT".to_string(),
		Token::CompLe => "<=".to_string(),
		Token::CompGe => ">=".to_string(),
		Token::TestEq => "==".to_string(),
		Token::TestNe => "!=".to_string(),
		Token::Arrow => "->".to_string(),
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
	let mut token = lexer.next_token();
	while token != Token::EofToken {
		let tok_str = token_to_string(&token);
		print!("{} ", tok_str);
		token = lexer.next_token();
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

mod lexer;
use crate::lexer::token::Token;
use std::{ env, fs, path::Path, process };

fn token_to_string(token: &Token) -> String {
	match token {
		Token::EofToken => "EOF".to_string(),
		Token::Unknown(c) => format!("Unknown {}", c).to_string(),
		Token::Identifier(s) => format!("Identifier {}", s).to_string(),
		Token::StringLiteral(s) => format!("StringLiteral {}", s).to_string(),
		Token::CharLiteral(s) => format!("CharLiteral {}", s).to_string(),
		Token::IntLiteral(n) => format!("IntLiteral {}", n).to_string(),
		Token::FloatLiteral(s) => format!("FloatLiteral {}", s).to_string(),
		Token::WordTrue => "WordTrue".to_string(),
		Token::WordFalse => "WordFalse".to_string(),
		Token::WordLet => "WordLet".to_string(),
		Token::WordReturn => "WordReturn".to_string(),
		Token::WordIf => "WordIf".to_string(),
		Token::WordThen => "WordThen".to_string(),
		Token::WordElse => "WordElse".to_string(),
		Token::WordElif => "WordElif".to_string(),
		Token::WordCase => "WordCase".to_string(),
		Token::WordOf => "WordOf".to_string(),
		Token::WordEnd => "WordEnd".to_string(),
		Token::WordWhile => "WordWhile".to_string(),
		Token::WordFor => "WordFor".to_string(),
		Token::WordIn => "WordIn".to_string(),
		Token::WordDo => "WordDo".to_string(),
		Token::WordDone => "WordDone".to_string(),
		Token::WordBreak => "WordBreak".to_string(),
		Token::WordContinue => "WordContinue".to_string(),
		Token::WordMatch => "WordMatch".to_string(),
		Token::WordWhen => "WordWhen".to_string(),
		Token::WordConst => "WordConst".to_string(),
		Token::WordType => "WordType".to_string(),
		Token::WordFn => "WordFn".to_string(),
		Token::WordRecord => "WordRecord".to_string(),
		Token::WordUnion => "WordUnion".to_string(),
		Token::WordTrait => "WordTrait".to_string(),
		Token::WordImpl => "WordImpl".to_string(),
		Token::WordAnd => "WordAnd".to_string(),
		Token::WordOr => "WordOr".to_string(),
		Token::WordXor => "WordXor".to_string(),
		Token::WordNot => "WordNot".to_string(),
		Token::CompLe => "CompLe".to_string(),
		Token::CompGe => "CompGe".to_string(),
		Token::TestEq => "TestEq".to_string(),
		Token::TestNe => "TestNe".to_string(),
		Token::Arrow => "Arrow".to_string(),
		Token::FwdCompose => "FwdCompose".to_string(),
		Token::ExclusiveRange => "ExclusiveRange".to_string(),
		Token::InclusiveRange => "InclusiveRange".to_string(),
		Token::ModuleAccess => "ModuleAccess".to_string(),
		Token::Concat => "Concat".to_string(),
		Token::AddAssign => "AddAssign".to_string(),
		Token::SubAssign => "SubAssign".to_string(),
		Token::MulAssign => "MulAssign".to_string(),
		Token::DivAssign => "DivAssign".to_string(),
		Token::ModAssign => "ModAssign".to_string(),
		Token::ParenL => "ParenL".to_string(),
		Token::ParenR => "ParenR".to_string(),
		Token::BracketL => "BracketL".to_string(),
		Token::BracketR => "BracketR".to_string(),
		Token::BraceL => "BraceL".to_string(),
		Token::BraceR => "BraceR".to_string(),
		Token::Dot => "Dot".to_string(),
		Token::Comma => "Comma".to_string(),
		Token::Semicolon => "Semicolon".to_string(),
		Token::Colon => "Colon".to_string(),
		Token::Equals => "Equals".to_string(),
		Token::Pipe => "Pipe".to_string(),
		Token::Bang => "Bang".to_string(),
		Token::Ampersand => "Ampersand".to_string(),
		Token::Hash => "Hash".to_string(),
		Token::Minus => "Minus".to_string(),
		Token::Plus => "Plus".to_string(),
		Token::Star => "Star".to_string(),
		Token::Slash => "Slash".to_string(),
		Token::Percent => "Percent".to_string(),
		Token::Less => "Less".to_string(),
		Token::Greater => "Greater".to_string(),
	}
}

fn print_tokens(source: &str) {
	let mut lexer = lexer::lexer::Lexer::new(source);

	let mut token = lexer.next_token();
	while match token {
		Token::EofToken => false,
		_ => true,
	} {
		println!("{}", token_to_string(&token));
		token = lexer.next_token();
	}
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

mod lexer;
use crate::lexer::token::Token;
use std::{ env, fs, path::Path, process };

fn token_to_string(token: &Token) -> String {
	match token {
		Token::EofToken => "EOF".to_string(),
		Token::Unknown(c) => format!("Unknown: {}", c).to_string(),
		Token::Identifier(s) => format!("Identifier: {}", s).to_string(),
		Token::StringLiteral(s) => format!("String: {}", s).to_string(),
		Token::CharLiteral(s) => format!("Char: {}", s).to_string(),
		Token::IntLiteral(n) => format!("Int: {}", n).to_string(),
		Token::FloatLiteral(s) => format!("Float: {}", s).to_string(),
		Token::WordTrue => "True".to_string(),
		Token::WordFalse => "False".to_string(),
		Token::WordLet => "Let".to_string(),
		Token::WordReturn => "Return".to_string(),
		Token::WordIf => "If".to_string(),
		Token::WordThen => "Then".to_string(),
		Token::WordElse => "Else".to_string(),
		Token::WordElif => "Elif".to_string(),
		Token::WordCase => "Case".to_string(),
		Token::WordOf => "Of".to_string(),
		Token::WordEnd => "End".to_string(),
		Token::WordWhile => "While".to_string(),
		Token::WordFor => "For".to_string(),
		Token::WordIn => "In".to_string(),
		Token::WordDo => "Do".to_string(),
		Token::WordDone => "Done".to_string(),
		Token::WordBreak => "Break".to_string(),
		Token::WordContinue => "Continue".to_string(),
		Token::WordMatch => "Match".to_string(),
		Token::WordWhen => "When".to_string(),
		Token::WordConst => "Const".to_string(),
		Token::WordType => "Type".to_string(),
		Token::WordFn => "Fn".to_string(),
		Token::WordRecord => "Record".to_string(),
		Token::WordUnion => "Union".to_string(),
		Token::WordTrait => "Trait".to_string(),
		Token::WordImpl => "Impl".to_string(),
		Token::WordAnd => "And".to_string(),
		Token::WordOr => "Or".to_string(),
		Token::WordXor => "Xor".to_string(),
		Token::WordNot => "Not".to_string(),
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

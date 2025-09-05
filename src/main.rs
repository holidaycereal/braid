mod parser;

use crate::parser::{ token::Token, lexer::Lexer };
use std::{ env, fs::File, path::Path, process, fmt };

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() < 2 || argv.len() > 3 {
        eprintln!("usage: {} <filename> [--dump]", argv[0]);
        process::exit(1);
    }

    let path = Path::new(&argv[1]);
    let dump_mode = argv.get(2).map(|s| s == "--dump").unwrap_or(false);

    let file = match File::open(path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("error opening file '{:?}': {}", path, e);
            process::exit(1);
        },
    };

    println!("braid lexer output for file {:?}:", path);

    let mut lexer = match Lexer::new(file) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("error creating lexer: {}", e);
            process::exit(1);
        },
    };

    loop {
        match lexer.next_token() {
            Ok(Ok(Some(tok))) => {
                if dump_mode {
                    // verbose dump: one token per line with position
                    println!("[line {}, col {}] {}", lexer.line, lexer.column, tok);
                } else {
                    // flat dump: all tokens space-separated
                    print!("{} ", tok);
                }
            },
            Ok(Ok(None)) => {
                if !dump_mode { println!(); }
                break;
            },
            Ok(Err(err)) => {
                eprintln!(
                    "syntax error at line {}, column {}: {:?}",
                    err.line, err.column, err.kind
                );
                process::exit(1);
            },
            Err(io_err) => {
                eprintln!("io error: {}", io_err);
                process::exit(1);
            },
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Literal(s)    => write!(f, "{}", s),

            Token::Let           => write!(f, "let"),
            Token::Var           => write!(f, "var"),
            Token::Return        => write!(f, "return"),
            Token::If            => write!(f, "if"),
            Token::Else          => write!(f, "else"),
            Token::Elif          => write!(f, "elif"),
            Token::While         => write!(f, "while"),
            Token::For           => write!(f, "for"),
            Token::In            => write!(f, "in"),
            Token::Break         => write!(f, "break"),
            Token::Continue      => write!(f, "continue"),
            Token::Match         => write!(f, "match"),
            Token::Fn            => write!(f, "fn"),
            Token::Type          => write!(f, "type"),
            Token::Record        => write!(f, "record"),
            Token::Union         => write!(f, "union"),
            Token::Iface         => write!(f, "iface"),
            Token::Impl          => write!(f, "impl"),
            Token::Where         => write!(f, "where"),

            Token::InclRange     => write!(f, "..="),
            Token::ConcatAssign  => write!(f, "++="),

            Token::ExclRange     => write!(f, ".."),
            Token::Concat        => write!(f, "++"),
            Token::CompLe        => write!(f, "<="),
            Token::CompGe        => write!(f, ">="),
            Token::TestEq        => write!(f, "=="),
            Token::TestNe        => write!(f, "!="),
            Token::Arrow         => write!(f, "->"),
            Token::LogicOr       => write!(f, "||"),
            Token::LogicAnd      => write!(f, "&&"),
            Token::FwdCompose    => write!(f, ">>"),
            Token::PathSeparator => write!(f, "::"),
            Token::AddAssign     => write!(f, "+="),
            Token::SubAssign     => write!(f, "-="),
            Token::MulAssign     => write!(f, "*="),
            Token::DivAssign     => write!(f, "/="),
            Token::ModAssign     => write!(f, "%="),
            Token::TernaryThen   => write!(f, "??"),
            Token::TernaryElse   => write!(f, "!!"),

            Token::ParenL        => write!(f, "("),
            Token::ParenR        => write!(f, ")"),
            Token::BracketL      => write!(f, "["),
            Token::BracketR      => write!(f, "]"),
            Token::BraceL        => write!(f, "{{"),
            Token::BraceR        => write!(f, "}}"),
            Token::Dot           => write!(f, "."),
            Token::Comma         => write!(f, ","),
            Token::Semicolon     => write!(f, ";"),
            Token::Colon         => write!(f, ":"),
            Token::Equals        => write!(f, "="),
            Token::Minus         => write!(f, "-"),
            Token::Plus          => write!(f, "+"),
            Token::Star          => write!(f, "*"),
            Token::Slash         => write!(f, "/"),
            Token::Percent       => write!(f, "%"),
            Token::Less          => write!(f, "<"),
            Token::Greater       => write!(f, ">"),
            Token::Bang          => write!(f, "!"),
            Token::Pipe          => write!(f, "|"),
            Token::Ampersand     => write!(f, "&"),
            Token::Caret         => write!(f, "^"),
        }
    }
}

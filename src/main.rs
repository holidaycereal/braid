mod lexer;
mod parser;
use crate::lexer::{ token::Token, lexer::Lexer };
use std::{ env, fs, path::Path, process, fmt };

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
                    for token in tokens { print!("{} ", token); }
                    println!();
                },
                Err(e) => {
                    eprintln!("syntax error: {:?}", e);
                    process::exit(1);
                },
            }
        },

        Err(e) => {
            eprintln!("error reading file '{:?}': {}", path, e);
            process::exit(1);
        },
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Token::Identifier(s) => write!(f, "{}", s),

            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::CharLiteral(s) => write!(f, "'{}'", s),

            Token::IntLiteral { base, int } => match base {
                2 => write!(f, "0b{}", int),
                8 => write!(f, "0o{}", int),
                16 => write!(f, "0x{}", int),
                _ => write!(f, "{}", int),
            },
            Token::FracLiteral { int, frac } => write!(f,
                "{}.{}", int, frac
            ),
            Token::ExpLiteral { int, has_minus, exp } => write!(f,
                "{}e{}{}", int, if *has_minus { '-' } else { '+' }, exp
            ),
            Token::FracExpLiteral { int, frac, has_minus, exp } => write!(f,
                "{}.{}e{}{}", int, frac, if *has_minus { '-' } else { '+' }, exp
            ),

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
            Token::TernaryLeft   => write!(f, "??"),
            Token::TernaryRight  => write!(f, "!!"),

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

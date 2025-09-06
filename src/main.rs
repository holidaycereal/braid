mod parser;

use crate::parser::{ token::{ Token, TokenBase }, lexer::Lexer };
use std::{ env, fs::File, path::Path, process };

fn main() {
    let argv: Vec<String> = env::args().collect();
    if argv.len() < 2 || argv.len() > 3 {
        eprintln!("usage: {} <filename> [--brief]", argv[0]);
        process::exit(1);
    }

    let path = Path::new(&argv[1]);
    let brief = argv.get(2).map(|s| s == "--brief").unwrap_or(false);

    let file = match File::open(path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("error opening file {:?}: {}", path, e);
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
                if brief {
                    // brief: all tokens space-separated
                    print!("{} ", token_short_name(&tok));
                } else {
                    // verbose: one token per line with position
                    println!("{}:{}\t{:?}", tok.line, tok.column, tok.base);
                }
            },
            Ok(Ok(None)) => {
                if brief { println!(); }
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

fn token_short_name(token: &Token) -> String {
    match &token.base {
        TokenBase::Identifier(s) => String::from(s),
        TokenBase::Literal(s)    => String::from(s),

        TokenBase::Let           => String::from("let"),
        TokenBase::Var           => String::from("var"),
        TokenBase::Return        => String::from("return"),
        TokenBase::If            => String::from("if"),
        TokenBase::Else          => String::from("else"),
        TokenBase::Elif          => String::from("elif"),
        TokenBase::While         => String::from("while"),
        TokenBase::For           => String::from("for"),
        TokenBase::In            => String::from("in"),
        TokenBase::Break         => String::from("break"),
        TokenBase::Continue      => String::from("continue"),
        TokenBase::Match         => String::from("match"),
        TokenBase::Fn            => String::from("fn"),
        TokenBase::Type          => String::from("type"),
        TokenBase::Record        => String::from("record"),
        TokenBase::Union         => String::from("union"),
        TokenBase::Iface         => String::from("iface"),
        TokenBase::Impl          => String::from("impl"),
        TokenBase::Where         => String::from("where"),

        TokenBase::InclRange     => String::from("..="),
        TokenBase::ConcatAssign  => String::from("++="),

        TokenBase::ExclRange     => String::from(".."),
        TokenBase::Concat        => String::from("++"),
        TokenBase::CompLe        => String::from("<="),
        TokenBase::CompGe        => String::from(">="),
        TokenBase::TestEq        => String::from("=="),
        TokenBase::TestNe        => String::from("!="),
        TokenBase::Arrow         => String::from("->"),
        TokenBase::LogicOr       => String::from("||"),
        TokenBase::LogicAnd      => String::from("&&"),
        TokenBase::FwdCompose    => String::from(">>"),
        TokenBase::PathSeparator => String::from("::"),
        TokenBase::AddAssign     => String::from("+="),
        TokenBase::SubAssign     => String::from("-="),
        TokenBase::MulAssign     => String::from("*="),
        TokenBase::DivAssign     => String::from("/="),
        TokenBase::ModAssign     => String::from("%="),
        TokenBase::TernaryThen   => String::from("??"),
        TokenBase::TernaryElse   => String::from("!!"),

        TokenBase::ParenL        => String::from("("),
        TokenBase::ParenR        => String::from(")"),
        TokenBase::BracketL      => String::from("["),
        TokenBase::BracketR      => String::from("]"),
        TokenBase::BraceL        => String::from("{"),
        TokenBase::BraceR        => String::from("}"),
        TokenBase::Dot           => String::from("."),
        TokenBase::Comma         => String::from(","),
        TokenBase::Semicolon     => String::from(";"),
        TokenBase::Colon         => String::from(":"),
        TokenBase::Equals        => String::from("="),
        TokenBase::Minus         => String::from("-"),
        TokenBase::Plus          => String::from("+"),
        TokenBase::Star          => String::from("*"),
        TokenBase::Slash         => String::from("/"),
        TokenBase::Percent       => String::from("%"),
        TokenBase::Less          => String::from("<"),
        TokenBase::Greater       => String::from(">"),
        TokenBase::Bang          => String::from("!"),
        TokenBase::Pipe          => String::from("|"),
        TokenBase::Ampersand     => String::from("&"),
        TokenBase::Caret         => String::from("^"),
    }
}

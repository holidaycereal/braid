use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone)]
pub enum Token {
    Identifier(String),
    Literal(String),

    Let, Var, Return, If, Else, Elif, Match,
    While, For, In, Break, Continue,
    Fn, Type, Record, Union, Iface, Impl, Where,

    InclRange,      // ..=
    ConcatAssign,   // ++=
    ExclRange,      // ..
    Concat,         // ++
    CompLe,         // <=
    CompGe,         // >=
    TestEq,         // ==
    TestNe,         // !=
    Arrow,          // ->
    LogicOr,        // ||
    LogicAnd,       // &&
    FwdCompose,     // >>
    PathSeparator,  // ::
    AddAssign,      // +=
    SubAssign,      // -=
    MulAssign,      // *=
    DivAssign,      // /=
    ModAssign,      // %=
    TernaryThen,    // ??
    TernaryElse,    // !!

    ParenL, ParenR, BracketL, BracketR, BraceL, BraceR,
    Dot, Comma, Semicolon, Colon, Equals,
    Minus, Plus, Star, Slash, Percent, Less, Greater,
    Bang, Pipe, Ampersand, Caret,
}

static KEYWORD_MAP: OnceLock<HashMap<&'static str, Token>> = OnceLock::new();

pub fn get_keyword_map() -> &'static HashMap<&'static str, Token> {
    KEYWORD_MAP.get_or_init(|| HashMap::from([
        ("let",      Token::Let),
        ("var",      Token::Var),
        ("return",   Token::Return),
        ("if",       Token::If),
        ("else",     Token::Else),
        ("elif",     Token::Elif),
        ("while",    Token::While),
        ("for",      Token::For),
        ("in",       Token::In),
        ("break",    Token::Break),
        ("continue", Token::Continue),
        ("match",    Token::Match),
        ("fn",       Token::Fn),
        ("type",     Token::Type),
        ("record",   Token::Record),
        ("union",    Token::Union),
        ("iface",    Token::Iface),
        ("impl",     Token::Impl),
        ("where",    Token::Where),
    ]))
}

pub const SYMBOL_DEFS: &'static [(&str, Token)] = &[
    ("..=", Token::InclRange),
    ("++=", Token::ConcatAssign),

    ("..", Token::ExclRange),
    ("++", Token::Concat),
    ("<=", Token::CompLe),
    (">=", Token::CompGe),
    ("==", Token::TestEq),
    ("!=", Token::TestNe),
    ("->", Token::Arrow),
    ("||", Token::LogicOr),
    ("&&", Token::LogicAnd),
    (">>", Token::FwdCompose),
    ("::", Token::PathSeparator),
    ("+=", Token::AddAssign),
    ("-=", Token::SubAssign),
    ("*=", Token::MulAssign),
    ("/=", Token::DivAssign),
    ("%=", Token::ModAssign),
    ("??", Token::TernaryThen),
    ("!!", Token::TernaryElse),

    ("(", Token::ParenL),
    (")", Token::ParenR),
    ("[", Token::BracketL),
    ("]", Token::BracketR),
    ("{", Token::BraceL),
    ("}", Token::BraceR),
    (".", Token::Dot),
    (",", Token::Comma),
    (";", Token::Semicolon),
    (":", Token::Colon),
    ("=", Token::Equals),
    ("-", Token::Minus),
    ("+", Token::Plus),
    ("*", Token::Star),
    ("/", Token::Slash),
    ("%", Token::Percent),
    ("<", Token::Less),
    (">", Token::Greater),
    ("!", Token::Bang),
    ("|", Token::Pipe),
    ("&", Token::Ampersand),
    ("^", Token::Caret),
];

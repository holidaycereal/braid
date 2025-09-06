use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Clone, Debug)]
pub enum TokenBase {
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

#[derive(Clone, Debug)]
pub struct Token {
    pub base: TokenBase,
    pub line: usize,
    pub column: usize,
}

static KEYWORD_MAP: OnceLock<HashMap<&'static str, TokenBase>> = OnceLock::new();

pub fn get_keyword_map() -> &'static HashMap<&'static str, TokenBase> {
    KEYWORD_MAP.get_or_init(|| HashMap::from([
        ("let",      TokenBase::Let),
        ("var",      TokenBase::Var),
        ("return",   TokenBase::Return),
        ("if",       TokenBase::If),
        ("else",     TokenBase::Else),
        ("elif",     TokenBase::Elif),
        ("while",    TokenBase::While),
        ("for",      TokenBase::For),
        ("in",       TokenBase::In),
        ("break",    TokenBase::Break),
        ("continue", TokenBase::Continue),
        ("match",    TokenBase::Match),
        ("fn",       TokenBase::Fn),
        ("type",     TokenBase::Type),
        ("record",   TokenBase::Record),
        ("union",    TokenBase::Union),
        ("iface",    TokenBase::Iface),
        ("impl",     TokenBase::Impl),
        ("where",    TokenBase::Where),
    ]))
}

pub const SYMBOL_DEFS: &'static [(&str, TokenBase)] = &[
    ("..=", TokenBase::InclRange),
    ("++=", TokenBase::ConcatAssign),

    ("..", TokenBase::ExclRange),
    ("++", TokenBase::Concat),
    ("<=", TokenBase::CompLe),
    (">=", TokenBase::CompGe),
    ("==", TokenBase::TestEq),
    ("!=", TokenBase::TestNe),
    ("->", TokenBase::Arrow),
    ("||", TokenBase::LogicOr),
    ("&&", TokenBase::LogicAnd),
    (">>", TokenBase::FwdCompose),
    ("::", TokenBase::PathSeparator),
    ("+=", TokenBase::AddAssign),
    ("-=", TokenBase::SubAssign),
    ("*=", TokenBase::MulAssign),
    ("/=", TokenBase::DivAssign),
    ("%=", TokenBase::ModAssign),
    ("??", TokenBase::TernaryThen),
    ("!!", TokenBase::TernaryElse),

    ("(", TokenBase::ParenL),
    (")", TokenBase::ParenR),
    ("[", TokenBase::BracketL),
    ("]", TokenBase::BracketR),
    ("{", TokenBase::BraceL),
    ("}", TokenBase::BraceR),
    (".", TokenBase::Dot),
    (",", TokenBase::Comma),
    (";", TokenBase::Semicolon),
    (":", TokenBase::Colon),
    ("=", TokenBase::Equals),
    ("-", TokenBase::Minus),
    ("+", TokenBase::Plus),
    ("*", TokenBase::Star),
    ("/", TokenBase::Slash),
    ("%", TokenBase::Percent),
    ("<", TokenBase::Less),
    (">", TokenBase::Greater),
    ("!", TokenBase::Bang),
    ("|", TokenBase::Pipe),
    ("&", TokenBase::Ampersand),
    ("^", TokenBase::Caret),
];

use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Identifier(String),

    StringLiteral(String),
    CharLiteral(String),

    IntLiteral {
        base: u32,
        int: String,
    },
    FracLiteral {
        int: String,
        frac: String,
    },
    ExpLiteral {
        int: String,
        has_minus: bool,
        exp: String,
    },
    FracExpLiteral {
        int: String,
        frac: String,
        has_minus: bool,
        exp: String,
    },

    WordLet, WordVar, WordReturn,
    WordIf, WordElse, WordElif,
    WordMatch, WordWhere, WordFn,
    WordWhile, WordFor, WordIn, WordBreak, WordContinue,
    WordType, WordRecord, WordUnion, WordIface, WordImpl,

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
    TernaryLeft,    // ??
    TernaryRight,   // !!

    ParenL, ParenR, BracketL, BracketR, BraceL, BraceR,
    Dot, Comma, Semicolon, Colon, Equals,
    Minus, Plus, Star, Slash, Percent, Less, Greater,
    Bang, Pipe, Ampersand, Caret,
}

static KEYWORD_MAP: OnceLock<HashMap<&'static str, Token>> = OnceLock::new();

pub fn get_keyword_map() -> &'static HashMap<&'static str, Token> {
    KEYWORD_MAP.get_or_init(|| HashMap::from([
        ("let",      Token::WordLet),
        ("var",      Token::WordVar),
        ("return",   Token::WordReturn),
        ("if",       Token::WordIf),
        ("else",     Token::WordElse),
        ("elif",     Token::WordElif),
        ("match",    Token::WordMatch),
        ("where",    Token::WordWhere),
        ("fn",       Token::WordFn),
        ("while",    Token::WordWhile),
        ("for",      Token::WordFor),
        ("in",       Token::WordIn),
        ("break",    Token::WordBreak),
        ("continue", Token::WordContinue),
        ("type",     Token::WordType),
        ("record",   Token::WordRecord),
        ("union",    Token::WordUnion),
        ("iface",    Token::WordIface),
        ("impl",     Token::WordImpl),
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
    ("??", Token::TernaryLeft),
    ("!!", Token::TernaryRight),

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

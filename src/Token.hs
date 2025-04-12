module Token where

data Token =
    Identifier String
  | StringLiteral String
  | CharLiteral String
  | NumLiteral String

  -- keywords
  -- top-level definitions
  | Def | Type | Record | Union | Trait | Iface | Impl
  -- control flow
  | Begin | End
  | Let | Return | Try
  | If | Then | Else | Elif | Case | When
  | Loop | While | For | In | Do | Break | Continue
  -- expression/pattern keywords
  | Match | Of | And

  -- symbols
  | LineComment | BlockComment
  | Concat | ConcatAssign
  | ERange | IRange
  | Tilde -- pattern binding
  | Caret -- pointer operations
  | Ampersand -- list construction
  -- delimiters
  | Dot | Comma | Semicolon | Colon | Pipe | Equals
  | ParenL | ParenR | BracketL | BracketR | BraceL | BraceR
  | Arrow
  | FwdCompose
  | PathSep
  -- logic
  | TestEq | TestNe | CmpLe | CmpGe | Less | Greater
  | LogicalAnd | LogicalOr | Bang
  | TernaryLeft | TernaryRight
  -- arithmetic
  | Plus | Minus | Star | Slash | Percent
  | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  deriving (Show, Eq)

keywordTokenDefs :: [(String, Token)]
keywordTokenDefs =
  [ ("def", Def)
  , ("type", Type)
  , ("record", Record)
  , ("union", Union)
  , ("trait", Trait)
  , ("iface", Iface)
  , ("impl", Impl)
  , ("begin", Begin)
  , ("end", End)
  , ("let", Let)
  , ("return", Return)
  , ("try", Try)
  , ("if", If)
  , ("then", Then)
  , ("else", Else)
  , ("elif", Elif)
  , ("case", Case)
  , ("when", When)
  , ("loop", Loop)
  , ("while", While)
  , ("for", For)
  , ("in", In)
  , ("do", Do)
  , ("break", Break)
  , ("continue", Continue)
  , ("match", Match)
  , ("of", Of)
  , ("and", And)
  ]

symbolTokenDefs :: [(String, Token)]
symbolTokenDefs =
  [ ("++=", ConcatAssign)
  , ("..=", IRange)
  , ("--", LineComment)
  , ("-*", BlockComment)
  , ("++", Concat)
  , ("..", ERange)
  , ("::", PathSep)
  , ("->", Arrow)
  , (">>", FwdCompose)
  , ("??", TernaryLeft)
  , ("!!", TernaryRight)
  , ("==", TestEq)
  , ("!=", TestNe)
  , ("<=", CmpLe)
  , (">=", CmpGe)
  , ("&&", LogicalAnd)
  , ("||", LogicalOr)
  , ("+=", AddAssign)
  , ("-=", SubAssign)
  , ("*=", MulAssign)
  , ("/=", DivAssign)
  , ("%=", ModAssign)
  , ("~", Tilde)
  , ("^", Caret)
  , ("&", Ampersand)
  , (".", Dot)
  , (",", Comma)
  , (";", Semicolon)
  , (":", Colon)
  , ("|", Pipe)
  , ("=", Equals)
  , ("(", ParenL)
  , (")", ParenR)
  , ("[", BracketL)
  , ("]", BracketR)
  , ("{", BraceL)
  , ("}", BraceR)
  , ("<", Less)
  , (">", Greater)
  , ("!", Bang)
  , ("+", Plus)
  , ("-", Minus)
  , ("*", Star)
  , ("/", Slash)
  , ("%", Percent)
  ]

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
  | If | Then | Else | Elif | Case
  | Loop | While | For | In | Do | Break | Continue
  -- expression keywords
  | Match | Of

  -- symbols
  | LineComment | BlockComment
  | Concat | ConcatAssign
  | ERange | IRange
  | Tilde -- pattern binding
  | Caret -- pointer operations
  -- delimiters
  | Dot | Comma | Semicolon | Colon | Pipe
  | ParenL | ParenR | BracketL | BracketR | BraceL | BraceR
  | Arrow
  | FwdCompose
  | PathSep
  -- logic
  | TestEq | TestNe | CmpLe | CmpGe | Less | Greater
  | And | Or | Bang
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
  , ("loop", Loop)
  , ("while", While)
  , ("for", For)
  , ("in", In)
  , ("do", Do)
  , ("break", Break)
  , ("continue", Continue)
  , ("match", Match)
  , ("of", Of)
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
  , ("&&", And)
  , ("||", Or)
  , ("+=", AddAssign)
  , ("-=", SubAssign)
  , ("*=", MulAssign)
  , ("/=", DivAssign)
  , ("%=", ModAssign)
  , ("|", Pipe)
  , ("~", Tilde)
  , ("^", Caret)
  , (".", Dot)
  , (",", Comma)
  , (";", Semicolon)
  , (":", Colon)
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

showToken :: Token -> String
showToken (Identifier s) = "Identifier(" ++ s ++ ")"
showToken (StringLiteral s) = "StringLiteral(\"" ++ s ++ "\")"
showToken (CharLiteral s) = "CharLiteral('" ++ s ++ "')"
showToken (NumLiteral s) = "NumLiteral(" ++ s ++ ")"
showToken token = show token

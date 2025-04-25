module Parser where

import Control.Monad ((>>=), (>=>))
import Token
import AST

type Parser = ([TopLevel], [Token])

data ParseError
  = ExpectedTopLevel Token
  | ExpectedStmt Token
  | ExpectedIdent Token
  | Expected [Token] Token
  | ExpectedIdentOr [Token] Token
  | UnexpectedEOF
  | InvalidField
  | InvalidVariant
  | ExpectedTypeExpr
  deriving Show

-- parse a source file
parse :: [Token] -> Either ParseError [TopLevel]
parse toks = aux ([], toks) where
    aux (acc, []) = return $ reverse acc
    aux (acc, tok:rest) = case tok of
        Const  -> parseConst     (acc, rest) >>= aux
        Def    -> parseFnDef     (acc, rest) >>= aux
        Type   -> parseTypeDef   (acc, rest) >>= aux
        Record -> parseRecordDef (acc, rest) >>= aux
        Union  -> parseUnionDef  (acc, rest) >>= aux
        Trait  -> parseTraitDef  (acc, rest) >>= aux
        Impl   -> parseImplBlock (acc, rest) >>= aux
        _      -> Left $ ExpectedTopLevel tok

--
-- parse top-level constructs
--

-- constant definitions
parseConst :: Parser -> Either ParseError Parser
parseConst (acc, rest) =
    expectIdent rest >>= \(name, rest) -> (
        expectToken Equals >=> parseExpr >=> \(expr, rest) ->
        expectToken Semicolon rest >>= \rest ->
        return (ConstDef name expr : acc, rest)
    ) rest

-- function definitions
parseFnDef :: Parser -> Either ParseError Parser
parseFnDef (acc, rest) =
    expectIdent rest >>= \(name, rest) ->
    parseParamList rest >>= \(params, rest) ->
    let finalise retType rest = case rest of
            Equals:rest -> parseExpr rest >>= \(expr, rest) ->
                           expectToken Semicolon rest >>= \rest ->
                           return (FnDef name params retType expr : acc, rest)
            BraceL:rest -> parseBlock rest >>= \(stmts, rest) ->
                           return (FnDef name params retType (Proc stmts) : acc, rest)
            tok:_       -> Left $ Expected [Equals, BraceL] tok
            []          -> Left UnexpectedEOF
    in
    case rest of
      Arrow:rest -> parseTypeExpr rest >>= \(retType, rest) ->
                    finalise (Just retType) rest
      _          -> finalise Nothing rest

-- helpers to parse function parameters
parseParam :: [Token] -> Either ParseError (Param, [Token])
parseParam = undefined

parseParamList :: [Token] -> Either ParseError ([Param], [Token])
parseParamList = aux []
  where aux acc (ParenL:rest) = parseParam rest >>= \(param, rest) ->
                                aux (param:acc) rest
        aux acc rest          = return (reverse acc, rest)

-- type aliases
parseTypeDef :: Parser -> Either ParseError Parser
parseTypeDef (acc, rest) =
    expectIdent rest >>= \(name, rest) ->
    parseTypeParams rest >>= \(tParams, rest) -> (
        expectToken Equals >=> parseTypeExpr >=> \(tExpr, rest) ->
        expectToken Semicolon rest >>= \rest ->
        return (TypeDef name tParams tExpr : acc, rest)
    ) rest

-- record types
parseRecordDef :: Parser -> Either ParseError Parser
parseRecordDef (acc, rest) =
    expectIdent rest >>= \(name, rest) ->
    parseTypeParams rest >>= \(tParams, rest) -> (
        expectToken BraceL >=> parseBody [] >=> \(fields, rest) ->
        return (RecordDef name tParams fields : acc, rest)
    ) rest
  where
    parseBody fields rest = case rest of
        BraceR                  : rest -> return (reverse fields, rest)
        Comma                   : rest -> parseBody fields rest
        Identifier name : Colon : rest -> parseTypeExpr rest >>= \(tExpr, rest) ->
                                          parseBody ((name, tExpr) : fields) rest
        _:_                            -> Left InvalidField
        []                             -> Left UnexpectedEOF

-- union types
parseUnionDef :: Parser -> Either ParseError Parser
parseUnionDef (acc, rest) =
    expectIdent rest >>= \(name, rest) ->
    parseTypeParams rest >>= \(tParams, rest) -> (
        expectToken BraceL >=> parseBody [] >=> \(variants, rest) ->
        return (UnionDef name tParams variants : acc, rest)
    ) rest
  where
    parseBody variants rest = case rest of
        BraceR                   : rest -> return (reverse variants, rest)
        Comma                    : rest -> parseBody variants rest
        Identifier name : ParenL : rest -> parseTupleType rest >>= \(tExpr, rest) ->
                                           parseBody ((name, Just tExpr) : variants) rest
        Identifier name          : rest -> parseBody ((name, Nothing)    : variants) rest
        _:_                             -> Left InvalidVariant
        []                              -> Left UnexpectedEOF

-- trait definitions
parseTraitDef :: Parser -> Either ParseError Parser
parseTraitDef = undefined

-- trait/interface implementation blocks
parseImplBlock :: Parser -> Either ParseError Parser
parseImplBlock = undefined

-- helper to parse type parameters
parseTypeParams :: [Token] -> Either ParseError ([String], [Token])
parseTypeParams (Less:rest) =
    expectIdent rest >>= \(name, rest) ->
    let loop acc rest = case rest of
            Greater         : rest -> return (reverse acc, rest)
            Comma           : rest -> loop acc rest
            Identifier name : rest -> loop (name:acc) rest
            tok:_                  -> Left $ ExpectedIdentOr [Greater, Comma] tok
            []                     -> Left UnexpectedEOF
    in
    loop [name] rest
parseTypeParams rest = return ([], rest)

--
-- expressions, type expressions, statements
--

-- parse an expression
parseExpr :: [Token] -> Either ParseError (Expr, [Token])
parseExpr = undefined

-- parse a type expression
parseTypeExpr :: [Token] -> Either ParseError (TypeExpr, [Token])
parseTypeExpr rest =
    parseFnType [] rest >>= \(types, rest) ->
    case types of
      [typ]    -> return (typ, rest)
      typ:typs -> return (FnType (typ:typs), rest)
      []       -> Left ExpectedTypeExpr

parseFnType :: [TypeExpr] -> [Token] -> Either ParseError ([TypeExpr], [Token])
parseFnType acc rest =
    parseSingleType rest >>= \(typ, rest) ->
    case rest of
      Arrow:rest -> parseFnType (typ:acc) rest
      _          -> return (reverse (typ:acc), rest)

parseSingleType :: [Token] -> Either ParseError (TypeExpr, [Token])
parseSingleType rest = case rest of
    ParenL : rest                 -> parseTupleType rest
    Identifier name : Less : rest -> parseTypeList Greater rest >>= \(types, rest) ->
                                     return (ConstructorApp name types, rest)
    Identifier name : rest        -> return (TypeReference name, rest)
    []                            -> Left UnexpectedEOF
    _                             -> Left ExpectedTypeExpr

parseTupleType :: [Token] -> Either ParseError (TypeExpr, [Token])
parseTupleType rest =
    parseTypeList ParenR rest >>= \(types, rest) ->
    return (TupleType types, rest)

parseTypeList :: Token -> [Token] -> Either ParseError ([TypeExpr], [Token])
parseTypeList delim = aux [] where
    aux acc (tok:rest)
      | tok == delim = return (reverse acc, rest)
      | tok == Comma = aux acc rest
    aux acc rest     = parseSingleType rest >>= \(typ, rest) ->
                       aux (typ:acc) rest

-- parse a statement
parseStmt :: [Token] -> Either ParseError (Stmt, [Token])
parseStmt = undefined

parseBlock :: [Token] -> Either ParseError ([Stmt], [Token])
parseBlock = aux [] where
    aux acc (BraceL:rest)    = return (reverse acc, rest)
    aux acc (Semicolon:rest) = aux acc rest
    aux acc rest             = parseStmt rest >>= \(stmt, rest) ->
                               aux (stmt:acc) rest

--
-- very general helpers
--

-- expect a certain token and consume it
expectToken :: Token -> [Token] -> Either ParseError [Token]
expectToken expected (actual:rest)
  | expected == actual = return rest
  | otherwise          = Left $ Expected [expected] actual
expectToken _ []       = Left UnexpectedEOF

-- expect an identifier
expectIdent :: [Token] -> Either ParseError (String, [Token])
expectIdent (Identifier name : rest) = return (name, rest)
expectIdent (tok:_)                  = Left $ ExpectedIdent tok
expectIdent []                       = Left UnexpectedEOF

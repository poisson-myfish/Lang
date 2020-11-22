module Parser where
import Types
import Lexer

data Expression = ExprFunDecl String DataType [Expression]  -- name, data type, arguments
                | ExprFunCall String [Expression]           -- name, arguments
                | ExprVarType String DataType  -- name, data type
                | ExprVarDecl Expression Expression  -- variable name + type (ExprVarType), value
                | ExprValue String DataType
                | ExprStart
                | ExprEnd
                | ExprRet String DataType  -- value, type
                | ExprNone
                | ExprSemicolon
                | ExprNothing
                deriving (Show, Eq)


parseVarTypeDecl :: [Token] -> (Expression, [Token])
parseVarTypeDecl [] = (ExprNone, [])
parseVarTypeDecl (TokenId "Nothing" : ts) = (ExprNothing, ts)
parseVarTypeDecl ts =
  let (TokenId name : TokenColon : TokenId dtype : ts') = ts
  in
    (ExprVarType name (getDataType dtype), ts')

parseFunParams :: [Token] -> ([Expression], [Token])
parseFunParams (TokenParenRight : ts) = ([], ts)
parseFunParams (TokenComma : ts) = (e : es, ts'')
  where
    (e, ts') = parseVarTypeDecl ts
    (es, ts'') = parseFunParams ts'

parseFunArgs :: [Token] -> ([Expression], [Token])
parseFunArgs (TokenComma : TokenId t : ts) = ((ExprVarType t TypeUnknown) : es, ts')
  where
    (es, ts') = parseFunArgs ts
parseFunArgs (TokenComma : TokenString t : ts) = ((ExprVarType t TypeString) : es, ts')
  where
    (es, ts') = parseFunArgs ts
parseFunArgs (TokenComma : TokenNumber t : ts) = ((ExprVarType (show t) TypeInt) : es, ts')
  where
    (es, ts') = parseFunArgs ts
parseFunArgs ts = ([], tail ts)

parseFunDecl :: [Token] -> (Expression, [Token])
parseFunDecl ts =
  let ((TokenId name) : TokenDoubleColon : (TokenId dtype) : TokenParenLeft : ts') = ts
      (args, ts'') = parseFunParams (TokenComma : ts')  -- Prepending comma because of impl of parseFunArgs
  in
    (ExprFunDecl name (getDataType dtype) args, ts'')

parseFunCall :: String -> [Token] -> (Expression, [Token])
parseFunCall name ts = (ExprFunCall name es, ts')
  where
    (es, ts') = parseFunArgs (TokenComma : ts)

parseRet :: [Token] -> (Expression, [Token])
parseRet [] = (ExprNone, [])
parseRet (TokenId name : ts') = (ExprRet name TypeUnknown, ts')
parseRet (TokenNumber num : ts') = (ExprRet (show num) TypeInt, ts')
parseRet (TokenString str : ts') = (ExprRet str TypeString, ts')

parseId :: [Token] -> (Expression, [Token])
parseId [] = (ExprNone, [])
parseId (TokenId id : TokenParenLeft : ts) = (e, ts')
  where
    (e, ts') = parseFunCall id ts

parseVarDecl :: [Token] -> (Expression, [Token])
parseVarDecl [] = (ExprNone, [])
parseVarDecl (TokenId name : TokenColon : TokenId dtype : TokenEquals : TokenId val : TokenParenLeft : ts) =
  let (vt, ts') = parseVarTypeDecl (TokenId name : TokenColon : TokenId dtype :
                                                  TokenEquals : TokenId val : ts)
  in
    (ExprVarDecl vt fc, ts'')  where (fc, ts'') = parseFunCall val ts
parseVarDecl (TokenId name : TokenColon : TokenId dtype : TokenEquals : TokenId val : ts) =
  let (vt, ts') = parseVarTypeDecl (TokenId name : TokenColon : TokenId dtype :
                                                      TokenEquals : TokenId val : ts)
  in
    (ExprVarDecl vt (ExprValue val TypeUnknown), ts)
parseVarDecl ts =
  let ((ExprVarType name dtype), ts') = parseVarTypeDecl ts
  in
    case dtype of
      TypeInt -> (e, ts'')  where (TokenEquals : (TokenNumber v) : ts'') = ts'
                                  e = ExprVarDecl (ExprVarType name dtype) (ExprValue (show v) TypeInt)
      TypeString -> (e, ts'')  where (TokenEquals : (TokenString v) : ts'') = ts'
                                     e = ExprVarDecl (ExprVarType name dtype) (ExprValue v TypeString)

parse :: [Token] -> [Expression]
parse [] = []
parse (t : ts) =
  case t of
    TokenFun -> es : parse ts'  where (es, ts') = parseFunDecl ts
    TokenCurlyLeft -> ExprStart : parse ts
    TokenCurlyRight -> ExprEnd : parse ts
    TokenRet -> es : parse ts'  where (es, ts') = parseRet ts
    TokenSemicolon -> ExprSemicolon : parse ts
    (TokenId _) -> es : parse ts'  where (es, ts') = parseId (t : ts)
    TokenLet -> es : parse ts'  where (es, ts') = parseVarDecl (ts)
    _ -> parse ts

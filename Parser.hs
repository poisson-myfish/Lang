module Parser where
import Types
import Lexer

data Expression = ExprFunDecl String DataType [Expression]  -- name, data type, arguments
                | ExprFunCall String           -- name
                | ExprVarTypeDecl String DataType  -- name, data type
                | ExprStart
                | ExprEnd
                | ExprRet String DataType  -- value, type
                | ExprNone
                | ExprSemicolon
                | ExprNothing
                deriving (Show, Eq)


parseVarTypeDecl :: [Token] -> (Expression, [Token])
parseVarTypeDecl [] = (ExprNone, [])
parseVarTypeDecl (TokenNothing : ts) = (ExprNothing, ts)
parseVarTypeDecl ts =
  let (TokenId name : TokenColon : TokenId dtype : ts') = ts
  in
    (ExprVarTypeDecl name (getDataType dtype), ts')

parseFunArgs :: [Token] -> ([Expression], [Token])
parseFunArgs (TokenParenRight : ts) = ([], ts)
parseFunArgs (TokenComma : ts) = (e : es, ts'')
  where
    (e, ts') = parseVarTypeDecl ts
    (es, ts'') = parseFunArgs ts'

parseFunDecl :: [Token] -> (Expression, [Token])
parseFunDecl ts =
  let ((TokenId name) : TokenDoubleColon : (TokenId dtype) : TokenParenLeft : ts') = ts
      (args, ts'') = parseFunArgs (TokenComma : ts')  -- Prepending comma because of impl of parseFunArgs
  in
    (ExprFunDecl name (getDataType dtype) args, ts'')

parseRet :: [Token] -> (Expression, [Token])
parseRet [] = (ExprNone, [])
parseRet (TokenId name : ts') = (ExprRet name TypeUnknown, ts')
parseRet (TokenNumber num : ts') = (ExprRet (show num) TypeInt, ts')
parseRet (TokenString str : ts') = (ExprRet str TypeString, ts')

parseId :: [Token] -> (Expression, [Token])
parseId [] = (ExprNone, [])

parse :: [Token] -> [Expression]
parse [] = []
parse (t : ts) =
  case t of
    TokenFun -> (fst fd) : parse (snd fd)  where fd = parseFunDecl ts
    TokenCurlyLeft -> ExprStart : parse ts
    TokenCurlyRight -> ExprEnd : parse ts
    TokenRet -> (fst ret) : parse (snd ret)  where ret = parseRet ts
    TokenSemicolon -> ExprSemicolon : parse ts
    _ -> parse ts

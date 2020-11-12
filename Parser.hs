module Parser where
import Types
import Lexer

data Expression = ExprFunDecl String DataType  -- name, data type
                | ExprFunCall String           -- name
                | ExprStart
                | ExprEnd
                | ExprRet String DataType  -- value, type
                | ExprNone
                | ExprSemicolon
                deriving (Show, Eq)


parseFunDecl :: [Token] -> (Expression, [Token])
parseFunDecl ts =
  let ((TokenId name) : TokenDoubleColon : (TokenId dtype) : TokenParenLeft : TokenParenRight : ts') = ts
  in
    (ExprFunDecl name (getDataType dtype), ts')

parseRet :: [Token] -> (Expression, [Token])
parseRet [] = (ExprNone, [])
parseRet (TokenId name : ts') = (ExprRet name TypeUnknown, ts')
parseRet (TokenNumber num : ts') = (ExprRet (show num) TypeInt, ts')
parseRet (TokenString str : ts') = (ExprRet str TypeString, ts')

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

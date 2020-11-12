module Lexer where
import Data.Char

data Operator = Plus | Minus | Mul | Div | IsEq
  deriving (Show, Eq)

data Token = TokenNumber Int
           | TokenOperator Operator
           | TokenId String
           | TokenString String
           | TokenFun
           | TokenColon
           | TokenDoubleColon
           | TokenParenLeft
           | TokenParenRight
           | TokenCurlyLeft
           | TokenCurlyRight
           | TokenSemicolon
           | TokenComma
           | TokenRet
  deriving (Show, Eq)


collect :: (Char -> Bool) -> String -> String -> (String, String)
collect cond s cs =
  if cond c then
    collect cond (s ++ [c]) (drop 1 cs)
  else
    (s, cs)
  where
    c = cs !! 0

collectNumber :: String -> String -> (String, String)
collectNumber n cs = collect isDigit n cs

collectId :: String -> String -> (String, String)
collectId id cs = collect isAlphaNum id cs

collectComment :: String -> String -> (String, String)
collectComment c cs = collect (/= '\n') c cs

collectString :: String -> String -> (String, String)
collectString c cs = collect (/= '"') c cs

collectColon :: String -> String -> (String, String)
collectColon c cs = collect (== ':') c cs

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isDigit c = stringToNumberT (fst num) : tokenize (snd num)
  | isAlpha c = stringToIdT (fst id)      : tokenize (snd id)
  | isSpace c = tokenize cs
  | c == '%'  = tokenize (snd comment)
  | c == '"'  = stringToStringT (fst str) : tokenize (drop 1 $ snd str)
  | c == ':'  = makeColonT (fst colon)    : tokenize (snd colon)
  | c == '('  = TokenParenLeft            : tokenize cs
  | c == ')'  = TokenParenRight           : tokenize cs
  | c == '{'  = TokenCurlyLeft            : tokenize cs
  | c == '}'  = TokenCurlyRight           : tokenize cs
  | c == ';'  = TokenSemicolon            : tokenize cs
  | c == ','  = TokenComma                : tokenize cs
  | otherwise = error "Cannot tokenize character"
  where
    num = collectNumber [c] cs
    id = collectId [c] cs
    comment = collectComment [c] cs
    str = collectString [(cs !! 0)] (drop 1 cs)
    colon = collectColon [c] cs



--- X To Token Converters

stringToNumberT :: String -> Token
stringToNumberT s = TokenNumber $ read s

stringToIdT :: String -> Token
stringToIdT "fun" = TokenFun
stringToIdT "return" = TokenRet
stringToIdT s = TokenId s

stringToStringT :: String -> Token
stringToStringT s = TokenString s

makeColonT :: String -> Token
makeColonT "::" = TokenDoubleColon
makeColonT ":" = TokenColon

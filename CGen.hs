module CGen where
import CTypes
import Parser
import Types


generateFunDecl :: Expression -> String
generateFunDecl (ExprFunDecl n dt) = (cType dt) ++ " " ++ n
  ++ "(" ++ ")"  -- TODO: Add parameters

generateStart :: String
generateStart = "{"

generateEnd :: String
generateEnd = "}"

generateRet :: Expression -> String
generateRet (ExprRet v dt) =
  case dt of
    TypeInt -> "return " ++ v
    TypeString -> "return \"" ++ v ++ "\"" -- Adding the quotes for the string
    TypeUnknown -> "return " ++ v

generateSemicolon :: String
generateSemicolon = ";"

generate :: [Expression] -> String 
generate [] = []
generate (e : es) =
  case e of
    (ExprFunDecl _ _) -> (generateFunDecl e) ++ (generate es)
    ExprStart -> generateStart ++ (generate es)
    ExprEnd -> generateEnd ++ (generate es)
    ExprSemicolon -> generateSemicolon ++ (generate es)
    (ExprRet _ _) -> (generateRet e) ++ (generate es)
    _ -> generate es

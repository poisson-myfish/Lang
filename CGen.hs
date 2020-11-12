module CGen where
import CTypes
import Parser


generateFunDecl :: Expression -> String
generateFunDecl (ExprFunDecl n dt) = (cType dt) ++ " " ++ n
  ++ "(" ++ ")"  -- TODO: Add parameters

generateStart :: String
generateStart = "{"

generateEnd :: String
generateEnd = "}"

generateRet :: Expression -> String
generateRet (ExprRet v) = "return " ++ v

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
    (ExprRet _) -> (generateRet e) ++ (generate es)
    _ -> generate es

module CGen where
import CTypes
import Parser
import Types

generateFunArgs :: [Expression] -> String
generateFunArgs (ExprNothing : es) = "void "
generateFunArgs (ExprVarTypeDecl name dtype : es) = (cType dtype) ++ " " ++ name ++ "," ++
  generateFunArgs es
generateFunArgs _ = ""

generateFunDecl :: Expression -> String
generateFunDecl (ExprFunDecl n dt args) = (cType dt) ++ " " ++ n
  ++ "(" ++ (init $ generateFunArgs args) ++ ")"

generateStart :: String
generateStart = " {\n"

generateEnd :: String
generateEnd = "\n}\n"

generateRet :: Expression -> String
generateRet (ExprRet v dt) = "return " ++ (cValue v dt)

generateSemicolon :: String
generateSemicolon = ";"

generate :: [Expression] -> String 
generate [] = []
generate (e : es) =
  case e of
    (ExprFunDecl _ _ _) -> (generateFunDecl e) ++ (generate es)
    ExprStart -> generateStart ++ (generate es)
    ExprEnd -> generateEnd ++ (generate es)
    ExprSemicolon -> generateSemicolon ++ (generate es)
    (ExprRet _ _) -> (generateRet e) ++ (generate es)
    _ -> generate es

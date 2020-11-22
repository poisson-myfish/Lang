module CGen where
import CTypes
import Parser
import Types

removeFunComma :: String -> String
removeFunComma [] = ""
removeFunComma [c] = ""
removeFunComma x = init x

generateFunParams :: [Expression] -> String
generateFunParams (ExprNothing : es) = "void "
generateFunParams (ExprVarType name dtype : es) = (cType dtype) ++ " " ++ name ++ "," ++
  generateFunParams es
generateFunParams _ = ""

generateFunArgs :: [Expression] -> String
generateFunArgs (ExprVarType name dtype : es) = (cValue name dtype) ++ "," ++ generateFunArgs es
generateFunArgs _ = ""

generateFunDecl :: Expression -> String
generateFunDecl (ExprFunDecl n dt args) = (cType dt) ++ " " ++ n
  ++ "(" ++ (removeFunComma $ generateFunParams args) ++ ")"

generateFunCall :: Expression -> String
generateFunCall (ExprFunCall name args) = name ++ "(" ++ (removeFunComma $ generateFunArgs args) ++ ")"

generateVarDecl :: Expression -> String
generateVarDecl (ExprVarDecl (ExprVarType name dt) (ExprFunCall val args)) =
                 (cType dt) ++ " " ++ name ++ " = " ++ (generateFunCall (ExprFunCall val args))
generateVarDecl (ExprVarDecl (ExprVarType name dtype) (ExprValue val vtype)) =
                 (cType dtype) ++ " " ++ name ++ " = " ++ (cValue val vtype)

generateStart :: String
generateStart = " {\n"

generateEnd :: String
generateEnd = "}\n\n"

generateRet :: Expression -> String
generateRet (ExprRet v dt) = "return " ++ (cValue v dt)

generateSemicolon :: String
generateSemicolon = ";\n"

generate :: [Expression] -> String 
generate [] = []
generate (e : es) =
  case e of
    (ExprFunDecl _ _ _) -> (generateFunDecl e) ++ (generate es)
    ExprStart -> generateStart ++ (generate es)
    ExprEnd -> generateEnd ++ (generate es)
    ExprSemicolon -> generateSemicolon ++ (generate es)
    (ExprFunCall _ _) -> generateFunCall e ++ (generate es)
    (ExprRet _ _) -> (generateRet e) ++ (generate es)
    (ExprVarDecl _ _) -> (generateVarDecl e) ++ (generate es)
    _ -> generate es

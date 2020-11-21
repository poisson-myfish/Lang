module CTypes where
import Types

cType :: DataType -> String
cType TypeInt = cInt
cType TypeString = cStr
cType (TypeCustom str) = str
cType TypeNothing = cNothing

cValue :: String -> DataType -> String
cValue s dt =
  case dt of
    TypeInt -> s
    TypeString -> "\"" ++ s ++ "\""
    TypeUnknown -> s

cInt :: String
cInt = "int"

cStr :: String
cStr = "char*"

cNothing :: String
cNothing = "void"

module CTypes where
import Types

cType :: DataType -> String
cType TypeInt = cInt
cType TypeString = cStr
cType (TypeCustom str) = str

cInt :: String
cInt = "int"

cStr :: String
cStr = "char*"

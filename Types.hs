module Types where

data DataType = TypeInt
              | TypeString
              | TypeCustom String
              | TypeNothing
              | TypeUnknown
    deriving (Show, Eq)

getDataType :: String -> DataType
getDataType str
  | str == intStr = TypeInt
  | str == stringStr = TypeString
  | str == nothingStr = TypeNothing
  | otherwise = TypeCustom str

intStr :: String
intStr = "Int"

stringStr :: String
stringStr = "String"

nothingStr :: String
nothingStr = "Nothing"

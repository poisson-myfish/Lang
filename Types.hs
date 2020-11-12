module Types where

data DataType = TypeInt
              | TypeString
              | TypeCustom String
              | TypeUnknown
    deriving (Show, Eq)

getDataType :: String -> DataType
getDataType str
  | str == intStr = TypeInt
  | str == stringStr = TypeString
  | otherwise = TypeCustom str

intStr :: String
intStr = "Int"

stringStr :: String
stringStr = "String"

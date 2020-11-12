module Main where
import Lexer
import Parser
import CGen

main :: IO ()
main = do
  file <- readFile "test.lang"
  tokens <- return (tokenize (file ++ " "))
  exprs <- return (parse tokens)
  out <- return (generate exprs)
  putStrLn $ out

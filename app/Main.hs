module Main where

import AST (dump)
import Parse.Combinators (Parser (runParser), eof)
import Parse.Grammar
import Parse.Lexer (lexerPosition, regularLexerState)

splitByChar :: String -> [String]
splitByChar [] = []
splitByChar s = foldr f ([[] | last s == '\n']) s
  where
    f '\n' toks = [] : toks
    f c [] = [[c]]
    f c toks@(_ : _) = (c : head toks) : tail toks

main :: IO ()
main = do
  let s = "auto v1;auto v2=1+*2;"
  case runParser (program <* eof) (regularLexerState s) of
    Left state -> print (lexerPosition state)
    Right (ast, _) -> putStrLn $ dump ast

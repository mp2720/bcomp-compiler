{-# LANGUAGE TypeFamilies #-}

module Parse.Lexer
  ( LexerState (..),
    regularLexerState,
    operator,
    ident,
    literal,
    keyword,
    pos,
    StringParser,
  )
where

import Control.Applicative (Alternative (many, (<|>)), asum)
import Control.Monad (unless)
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import Data.Function (on)
import Data.Functor (void)
import Data.List (elemIndex, foldl')
import Diagnostics (P (..))
import Parse.Combinators (Parser (..), State (..), failParser, mapTerm, notFollowedBy, satisfy, try)

data LexerState = LexerState
  { lexerInput :: String,
    lexerPosition :: P,
    lexerTabSize :: Int
  }

type StringParser = Parser LexerState

instance State LexerState where
  type Term LexerState = Char
  didAdvance = on (/=) (positionOffset . lexerPosition)

  next s@(LexerState [] _ _) = (Nothing, s)
  next s@(LexerState (c : cs) p tabLen) =
    ( Just c,
      s
        { lexerPosition = nextPosition,
          lexerInput = cs
        }
    )
    where
      nextPosition = Position nextOffset nextLine nextColumn
      nextOffset = positionOffset p + 1
      nextLine = case c of
        '\n' -> line + 1
        _ -> line
      nextColumn = case c of
        '\r' -> column
        '\n' -> 0
        '\t' -> (column `div` tabLen + 1) * tabLen
        _ -> column + 1

      line = positionLine p
      column = positionColumn p

regularLexerState :: String -> LexerState
regularLexerState s = LexerState s (Position 0 1 1) 8

char :: Char -> StringParser Char
char c = satisfy (== c)

string :: String -> StringParser String
string = traverse char

pos :: StringParser P
pos = Parser $ \s -> Right (lexerPosition s, s)

lexeme :: StringParser a -> StringParser a
lexeme p = ignore *> try p <* ignore

ignore :: StringParser ()
ignore = void $ many (void (satisfy isSpace) <|> void lineComment <|> void multilineComment)
  where
    -- '//' (^'\n')*
    lineComment = (++) <$> try (string "//") <*> many (satisfy (/= '\n'))
    -- '/*' rest
    multilineComment = (++) <$> try (string "/*") <*> rest
      where
        -- '*/' | . rest
        rest = try (string "*/") <|> (:) <$> satisfy (const True) <*> rest

operator :: String -> StringParser ()
operator expected =
  lexeme $
    asum
      ( map
          op
          [ "{",
            "}",
            "[",
            "]",
            "(",
            ")",
            ",",
            ";",
            ":",
            "-",
            "+",
            "*",
            "~",
            -- "||",
            -- "&&",
            "|",
            "&",
            "==",
            "=",
            "!=",
            -- "!",
            "<=",
            "<",
            ">=",
            ">",
            "^>=",
            "^>",
            "^<=",
            "^<"
          ]
      )
  where
    op s = do
      p <- try $ string s
      unless (p == expected) failParser

identFirstCh :: StringParser Char
identFirstCh = char '_' <|> satisfy isAlpha

identFollowingCh :: StringParser Char
identFollowingCh = identFirstCh <|> satisfy isDigit

identOrKeyword :: StringParser String
identOrKeyword = (:) <$> identFirstCh <*> many identFollowingCh

ident :: StringParser String
ident = lexeme $ do
  i <- identOrKeyword
  if isKeyword i then failParser else return i

keyword :: String -> StringParser ()
keyword expected = lexeme $ do
  i <- identOrKeyword
  unless (isKeyword i && i == expected) failParser

isKeyword :: String -> Bool
isKeyword =
  ( `elem`
      [ "if",
        "else",
        -- "while",
        "auto",
        "goto",
        "sizeof"
        -- "signext",
        -- "swaph",
        -- "shl",
        -- "shr",
        -- "asr"
      ]
  )

digit :: String -> StringParser Integer
digit digits = toInteger <$> mapTerm (\c -> toLower c `elemIndex` digits)

literal :: StringParser Integer
literal = lexeme $ lit' <* notFollowedBy identFollowingCh
  where
    lit' = (char '0' *> (hex <|> oct <|> bin <|> num' decDigits 10 0)) <|> num decDigits 10

    hex = char 'x' *> num "0123456789abcdef" 16
    oct = char 'o' *> num "01234567" 8
    bin = char 'b' *> num "01" 2

    decDigits = "0123456789"

    -- dig ('_'* dig)*
    num digits base = do
      d0 <- digit digits
      num' digits base d0

    -- ('_'* dig)*
    num' digits base p =
      foldl' (\a b -> base * a + b) p <$> many (many (char '_') *> digit digits)

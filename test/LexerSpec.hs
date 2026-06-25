module LexerSpec (spec) where

import Diagnostics (P (..))
import Control.Applicative (Alternative (many, (<|>)))
import Parse.Combinators (eof, runParser)
import Parse.Lexer
import Test.Hspec

r :: StringParser v -> String -> Either P v
r lexer s = case runParser lexer (regularLexerState s) of
  Left state -> Left $ lexerPosition state
  Right (v, _) -> Right v

spec :: Spec
spec =
  describe "Lexer" $ do
    it "identifiers and literals" $ do
      r ident "abc__123_.b" `shouldBe` Right "abc__123_"
      r ident "_" `shouldBe` Right "_"
      r (many literal <* eof) "1 00000_0 09 14 0xA1_bAf_e 0b1____100 " `shouldBe` Right [1, 0, 9, 14, 0xA1BAFE, 12]
      r literal "\t \t0o18" `shouldBe` Left (Position 3 1 16)
      r literal "001a" `shouldBe` Left (Position 0 1 1)
      r literal "001_" `shouldBe` Left (Position 0 1 1)
      r literal "0x_01/" `shouldBe` Left (Position 0 1 1)
    it "identifiers and keywords" $ do
      r (keyword "if") "if(" `shouldBe` Right ()
      r (keyword "if") "if_" `shouldBe` Left (Position 0 1 1)
      r ident "if_" `shouldBe` Right "if_"
    it "operators" $ do
      r (operator ">=") ">=" `shouldBe` Right ()
      r (operator "+") "+=" `shouldBe` Right ()
      r
        ( (,,,,)
            <$> ident
            <*> operator "+"
            <*> operator "-"
            <*> operator "-"
            <*> ident
        )
        "a+--b"
        `shouldBe` Right ("a", (), (), (), "b")
      r (operator ">") ">=" `shouldBe` Left (Position 0 1 1)
      r (("<=" <$ operator "<=") <|> ("<" <$ operator "<")) "<" `shouldBe` Right "<"
      r (operator "-" <* operator "~") "-~" `shouldBe` Right ()
    it "comments" $ do
      r ((,) <$> ident <*> ident) "    a//bc\n//\nd/   //e   " `shouldBe` Right ("a", "d")
      r
        ( (,)
            <$> ident
            <* operator "*"
            <*> ident
            <* eof
        )
        "a/******//*\n\n* /\n// */*b/**/"
        `shouldBe` Right ("a", "b")
      r ident "a/*\nb" `shouldBe` Left (Position 5 2 1)
    it "test lines count" $ do
      r ((,) <$> ident <*> literal) "a\n\r\n " `shouldBe` Left (Position 5 3 1)

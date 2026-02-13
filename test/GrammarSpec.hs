{-# LANGUAGE QuasiQuotes #-}

module GrammarSpec (spec) where

import AST
import Data.List (intercalate)
import Parse.Combinators (eof, runParser)
import Parse.Grammar
import Parse.Lexer (regularLexerState)
import Test.Hspec
import Text.RawString.QQ

t :: (StringDump v) => Rule v -> String -> Either () String
t rule s = case runParser rule (regularLexerState s) of
  Left _ -> Left ()
  Right (v, _) -> Right $ dump v

shouldBeIndentAgnostic :: Either () String -> Either () String -> Expectation
shouldBeIndentAgnostic real expected = fmap trimIndent real `shouldBe` fmap trimIndent expected
  where
    trimIndent s = intercalate "\n" $ map lineTrimIndent (lines s)
    lineTrimIndent [] = []
    lineTrimIndent (' ' : cs) = lineTrimIndent cs
    lineTrimIndent ('\t' : cs) = lineTrimIndent cs
    lineTrimIndent s = s

spec :: Spec
spec =
  describe "Grammar" $ do
    it "atom & lexpr" $ do
      t (atomExpr <* eof) "--~-&*a" `shouldBe` Right "--~-&*a"
      t (atomExpr <* eof) "*sizeof a " `shouldBe` Right "*(sizeof a)"
      t (atomExpr <* eof) "*sizeof(a)" `shouldBe` Right "*(sizeof a)"
      t (atomExpr <* eof) "*sizeof (a+1) " `shouldBe` Left ()
      t (atomExpr <* eof) "*((*a+&z)[i][j])" `shouldBe` Right "**(*((*a + &z) + i) + j)"
      t (atomExpr <* eof) "a[*i+1]" `shouldBe` Right "*(a + (*i + 1))"
      t (atomExpr <* eof) "&1" `shouldBe` Left ()
    it "rexpr" $ do
      t (rexpr <* eof) "-a - a |b" `shouldBe` Right "((-a - a) | b)"
      t (rexpr <* eof) "-a - a &b" `shouldBe` Right "(-a - (a & b))"
      t (rexpr <* eof) "a==b|a>=b" `shouldBe` Right "((a == (b | a)) >= b)"
      t (rexpr <* eof) "a!=b<c<=d^<sa^<=sb^>sc^>=(sd>z)"
        `shouldBe` Right "(((((((a != b) < c) <= d) ^< sa) ^<= sb) ^> sc) ^>= (sd > z))"
      t (rexpr <* eof) "sizeof a +1" `shouldBe` Right "((sizeof a) + 1)"
    it "lexpr" $ do
      t (lexpr <* eof) "1" `shouldBe` Left ()
      t (lexpr <* eof) "&a" `shouldBe` Left ()
      t (lexpr <* eof) "*&a" `shouldBe` Right "*&a"
      t (lexpr <* eof) "a[i][j]" `shouldBe` Right "*(*(a + i) + j)"
    it "stmt" $ do
      t (stmt <* eof) "if(a){L:if(b){goto L;}else goto L2;z[i]=1;}else a=1;"
        `shouldBeIndentAgnostic` Right
          [r|if(a) {
            L: if(b) {
              goto L;
            }
            else {
              goto L2;
            }
            *(z + i) = 1;
          }
          else {
            a = 1;
          }|]
    it "dangling else" $ do
      t (stmt <* eof) "if(a==1)if(b)goto L1;else goto L2;"
        `shouldBeIndentAgnostic` Right
          [r|if((a == 1)) {
            if(b) {
              goto L1;
            }
            else {
              goto L2;
            }
          }
          else {
            ;
          }|]
    it "blocks" $ do
      t (program <* eof) "{{}{a=1;{z=2;auto b = 9;}};}"
        `shouldBeIndentAgnostic` Right
          [r|{
          {
            
          }
          {
            a = 1;
            {
              z = 2;
              auto b = 9;
            }
          }
          ;
        }|]
    it "defs" $ do
      t (program <* eof) "auto v1;auto v2=1+*2;a[1];a[1]=1;b[]0;c[2]1,2;{}"
        `shouldBeIndentAgnostic` Right
          [r|auto v1;
          auto v2 = (1 + *2);
          a[1];
          *(a + 1) = 1;
          b[] 0;
          c[2] 1, 2;
          {
            
          }|]

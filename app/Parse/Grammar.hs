module Parse.Grammar
  ( Rule,
    parseProgram,
    program,
    stmt,
    arrDef,
    varDef,
    labeledStmt,
    execStmt,
    rexpr,
    lexpr,
    atomExpr,
  )
where

import AST
import Control.Applicative (Alternative (many, some, (<|>)), asum, optional)
import Data.Foldable (Foldable (foldl'))
import Diagnostics (Diagnostic (Error))
import Parse.Combinators
import Parse.Lexer

type Rule = StringParser

{-
This is the initial version of the grammar.
In future it's planned to make it more close to C and B languages.
Now it lacks:
  - inc/dec
  - assigns as expressions
  - (DONE) code blocks
  - loops and loop control
  - functions
  - switch (not guaranteed to be added)
-}

-- TODO: sometimes parser spits a syntax error with zero column position.
-- I understand the logic, but that is not what you would usually expect from a parser.
-- Not exactly sure if I should consider this a bug, so do some investigation and think about it.

parseProgram :: String -> ([Diagnostic], Program)
parseProgram s = case runParser (program <* eof) (regularLexerState s) of
  Left state -> ([Error (Just $ lexerPosition state) "syntax error"], Program [])
  Right (prog, _) -> ([], prog)

program :: Rule Program
program = Program <$> many stmt

stmt :: Rule Stmt
stmt =
  try arrDef
    <|> varDef
    <|> ExecStmt <$> pos <*> labeledStmt

arrDef :: Rule Stmt
arrDef = do
  p <- pos
  arrIdent <- ident
  size <- par "[" (optional literal) "]"
  elements <- join (operator ",") literal
  colon
  return $ ArrDef p arrIdent size elements

varDef :: Rule Stmt
varDef = do
  p <- pos
  keyword "auto"
  varIdent <- ident
  v <- optional (operator "=" *> rexpr)
  colon
  return $ VarDef p varIdent v

labeledStmt :: Rule LabeledStmt
labeledStmt = LabeledStmt <$> pos <*> optional (try $ ident <* operator ":") <*> execStmt

execStmt :: Rule ExecStmt
execStmt =
  (assign <|> goto) <* colon
    <|> if_
    <|> block
    <|> noOp
  where
    assign = Assign <$> pos <*> lexpr <* operator "=" <*> rexpr
    if_ = do
      p <- pos
      keyword "if"
      cond <- par "(" rexpr ")"
      then_ <- labeledStmt
      elsePos <- pos
      else_ <- keyword "else" *> labeledStmt <|> pure (LabeledStmt elsePos Nothing $ NoOp elsePos)
      return $ If p cond then_ else_
    goto = Goto <$> pos <* keyword "goto" <*> ident
    block = Block <$> pos <*> par "{" (many stmt) "}"
    noOp = NoOp <$> pos <* operator ";"

par :: String -> Rule a -> String -> Rule a
par l p r = operator l *> p <* operator r

{-
  Operator precedence:
   0: &
   1: | + -
   2: == != < <= > >= ~< ~<= ~> ~>=
-}

rexpr :: Rule RightExpr
rexpr = binOp2
  where
    binOp2 =
      leftAssoc
        RelOpApp
        binOp1
        $ operators
          [ (Equals, operator "=="),
            (NotEq, operator "!="),
            (Lt, operator "<"),
            (Leq, operator "<="),
            (Gt, operator ">"),
            (Geq, operator ">="),
            (UnsignedLt, operator "^<"),
            (UnsignedLeq, operator "^<="),
            (UnsignedGt, operator "^>"),
            (UnsignedGeq, operator "^>=")
          ]
    binOp1 =
      leftAssoc
        BinOpApp
        binOp0
        $ operators
          [ (BitOr, operator "|"),
            (Add, operator "+"),
            (Sub, operator "-")
          ]
    binOp0 = leftAssoc BinOpApp atomExpr $ operators [(BitAnd, operator "&")]
    -- for later use
    -- rightAssoc opnd ops =
    --   try
    --     ( do
    --         p <- pos
    --         l <- opnd
    --         op <- ops
    --         r <- leftAssoc opnd ops
    --         return $ BinOpApp p l op r
    --     )
    --     <|> opnd
    leftAssoc app opnd ops = do
      headOpnd <- opnd
      tailOpnds <- many ((,,) <$> pos <*> ops <*> opnd)
      return $ foldl' (\l (rPos, rOp, rOpnd) -> app rPos l rOp rOpnd) headOpnd tailOpnds

lexpr :: Rule LeftExpr
lexpr =
  try arrSub <|> deref <|> var
  where
    arrSub = do
      arr <- LeftExpr <$> pos <*> var <|> par "(" rexpr ")"
      (firstSub : tailSubs) <- some $ par "[" rexpr "]"
      return $ foldl' foldSub (derefFromArrSub arr firstSub) tailSubs
      where
        foldSub a = derefFromArrSub (LeftExpr (position a) a)
        derefFromArrSub a i = PtrDeref (position a) (BinOpApp (position a) a Add i)
    deref = PtrDeref <$> pos <* operator "*" <*> atomExpr
    var = Var <$> pos <*> ident

atomExpr :: Rule RightExpr
atomExpr =
  unOp
    <|> Sizeof <$> pos <* keyword "sizeof" <*> (ident <|> par "(" ident ")")
    <|> AddressOf <$> pos <* operator "&" <*> lexpr
    <|> LeftExpr <$> pos <*> lexpr
    <|> par "(" rexpr ")"
    <|> Literal <$> pos <*> literal
  where
    unOp =
      UnaryOpApp
        <$> pos
        <*> operators
          [ (BitNot, operator "~"),
            (Negate, operator "-")
          ]
        <*> atomExpr

operators :: [(a, Rule b)] -> Rule a
operators opMap = asum $ map (uncurry (<$)) opMap

colon :: Rule ()
colon = operator ";"

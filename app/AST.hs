{-# LANGUAGE FlexibleInstances #-}

module AST
  ( Program (..),
    Positioned (position),
    Ident,
    Stmt (..),
    LabeledStmt (..),
    ExecStmt (..),
    RightExpr (..),
    BinOp (..),
    RelOp (..),
    UnaryOp (..),
    LeftExpr (..),
    StringDump (dump),
  )
where

import Data.List (intercalate)
import Diagnostics (P)
import Text.Printf (printf)

newtype Program = Program [Stmt]
  deriving (Show, Eq)

type Ident = String

data Stmt
  = ExecStmt P LabeledStmt
  | VarDef P Ident (Maybe RightExpr)
  | ArrDef P Ident (Maybe Integer) [Integer]
  deriving (Show, Eq)

data LabeledStmt = LabeledStmt P (Maybe Ident) ExecStmt
  deriving (Show, Eq)

data ExecStmt
  = Assign P LeftExpr RightExpr
  | If P RightExpr LabeledStmt LabeledStmt
  | Goto P Ident
  | Block P [Stmt]
  | NoOp P
  deriving (Show, Eq)

data RightExpr
  = Literal P Integer
  | BinOpApp P RightExpr BinOp RightExpr
  | RelOpApp P RightExpr RelOp RightExpr
  | UnaryOpApp P UnaryOp RightExpr
  | AddressOf P LeftExpr
  | Sizeof P Ident
  | LeftExpr P LeftExpr
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | BitOr
  | BitAnd
  deriving (Show, Eq)

data RelOp
  = Equals
  | NotEq
  | Gt
  | Geq
  | Lt
  | Leq
  | UnsignedGt
  | UnsignedGeq
  | UnsignedLt
  | UnsignedLeq
  deriving (Show, Eq)

data UnaryOp = BitNot | Negate
  deriving (Show, Eq)

data LeftExpr
  = Var P Ident
  | PtrDeref P RightExpr
  deriving (Show, Eq)

class Positioned a where
  position :: a -> P

instance Positioned Stmt where
  position (ExecStmt p _) = p
  position (VarDef p _ _) = p
  position (ArrDef p _ _ _) = p

instance Positioned LabeledStmt where
  position (LabeledStmt p _ _) = p

instance Positioned ExecStmt where
  position (Assign p _ _) = p
  position (If p _ _ _) = p
  position (Goto p _) = p
  position (Block p _) = p
  position (NoOp p) = p

instance Positioned RightExpr where
  position (Literal p _) = p
  position (BinOpApp p _ _ _) = p
  position (RelOpApp p _ _ _) = p
  position (UnaryOpApp p _ _) = p
  position (AddressOf p _) = p
  position (Sizeof p _) = p
  position (LeftExpr p _) = p

instance Positioned LeftExpr where
  position (Var p _) = p
  position (PtrDeref p _) = p

class StringDump a where
  dump' :: String -> a -> String
  dump :: a -> String
  dump = dump' ""

-- TODO: indentation is broken in dump and the code looks awful
-- TODO: maybe use Reader and Writer monad to simplify?

instance StringDump Program where
  dump' _ (Program stmts) = dump stmts

instance StringDump Stmt where
  dump' indent stmt =
    case stmt of
      ExecStmt _ labeledStmt -> dump' indent labeledStmt
      VarDef _ varIdent rexpr -> indent ++ printf "auto %s%s;" varIdent (dumpAssignRexpr rexpr)
      ArrDef _ arrIdent size els -> indent ++ printf "%s[%s]%s;" arrIdent (dumpArrSize size) (dumpElements els)
    where
      dumpAssignRexpr Nothing = ""
      dumpAssignRexpr (Just v) = printf " = %s" (dump' indent v)

      dumpArrSize Nothing = ""
      dumpArrSize (Just size) = show size

      dumpElements els@(_ : _) = " " ++ intercalate ", " (map show els)
      dumpElements [] = ""

instance StringDump LabeledStmt where
  dump' indent (LabeledStmt _ Nothing stmt) = indent ++ dump' indent stmt
  dump' indent (LabeledStmt _ (Just label) stmt) = indent ++ printf "%s: %s" label (dump' indent stmt)

instance StringDump ExecStmt where
  dump' _ (Assign _ l r) = printf "%s = %s;" (dump l) (dump r)
  dump' indent (If _ cond then_ else_) =
    printf "if(%s) %s\nelse %s" (dump cond) (branch then_) (branch else_)
    where
      branch :: LabeledStmt -> String
      branch (LabeledStmt _ _ block@(Block _ _)) = dump' indent block
      branch stmt = printf "{\n%s\n}" (indent ++ dump' (indent ++ "  ") stmt)
  dump' _ (Goto _ labelIdent) = printf "goto %s;" labelIdent
  dump' _ (NoOp _) = ";"
  dump' indent (Block _ stmts) = printf "{\n%s\n%s}" (dump' (indent ++ "  ") stmts) indent

instance StringDump [Stmt] where
  dump' indent stmts = intercalate "\n" (map (dump' indent) stmts)

instance StringDump RightExpr where
  dump' _ (Literal _ int) = show int
  dump' _ (BinOpApp _ r op l) = printf "(%s %s %s)" (dump r) dumpOp (dump l)
    where
      dumpOp = case op of
        Add -> "+"
        Sub -> "-"
        BitOr -> "|"
        BitAnd -> "&"
  dump' _ (RelOpApp _ r op l) = printf "(%s %s %s)" (dump r) dumpOp (dump l)
    where
      dumpOp = case op of
        Equals -> "=="
        NotEq -> "!="
        Gt -> ">"
        Geq -> ">="
        Lt -> "<"
        Leq -> "<="
        UnsignedGt -> "^>"
        UnsignedGeq -> "^>="
        UnsignedLt -> "^<"
        UnsignedLeq -> "^<="
  dump' _ (UnaryOpApp _ op opnd) = dumpOp ++ dump opnd
    where
      dumpOp = case op of
        BitNot -> "~"
        Negate -> "-"
  dump' _ (AddressOf _ lval) = "&" ++ dump lval
  dump' _ (Sizeof _ arr) = printf "(sizeof %s)" arr
  dump' _ (LeftExpr _ lexpr) = dump lexpr

instance StringDump LeftExpr where
  dump' _ (Var _ ident) = ident
  dump' _ (PtrDeref _ rexpr) = "*" ++ dump rexpr

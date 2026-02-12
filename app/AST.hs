{-# LANGUAGE FlexibleInstances #-}

module AST
  ( P (..),
    Positioned (..),
    Program (..),
    LabelIdent,
    VarIdent,
    Stmt (..),
    ExecStmt (..),
    RightExpr (..),
    BinOp (..),
    UnaryOp (..),
    LeftExpr (..),
    StringDump (dump),
  )
where

import Data.List (intercalate)
import Text.Printf (printf)

data P = Position
  { positionOffset :: Int,
    positionLine :: Int,
    positionColumn :: Int
  }
  deriving (Eq)

instance Show P where
  show (Position _ line column) = printf "%d:%d" line column

newtype Program = Program [Stmt]
  deriving (Show, Eq)

type LabelIdent = String

type VarIdent = String

data Stmt
  = ExecStmt P (Maybe LabelIdent) ExecStmt
  | VarDef P VarIdent (Maybe RightExpr)
  | ArrDef P VarIdent (Maybe Integer) [Integer]
  deriving (Show, Eq)

data ExecStmt
  = Assign P LeftExpr RightExpr
  | If P RightExpr [Stmt] [Stmt]
  | Goto P LabelIdent
  deriving (Show, Eq)

data RightExpr
  = Literal P Integer
  | BinOpApp P RightExpr BinOp RightExpr
  | UnaryOpApp P UnaryOp RightExpr
  | AddressOf P LeftExpr
  | Sizeof P VarIdent
  | LeftExpr P LeftExpr
  deriving (Show, Eq)

data BinOp
  = Add
  | Sub
  | BitOr
  | BitAnd
  | Eq
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
  = Var P VarIdent
  | PtrDeref P RightExpr
  deriving (Show, Eq)

class Positioned a where
  position :: a -> P

instance Positioned Stmt where
  position (ExecStmt p _ _) = p
  position (VarDef p _ _) = p
  position (ArrDef p _ _ _) = p

instance Positioned ExecStmt where
  position (Assign p _ _) = p
  position (If p _ _ _) = p
  position (Goto p _) = p

instance Positioned RightExpr where
  position (Literal p _) = p
  position (BinOpApp p _ _ _) = p
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

instance StringDump Program where
  dump' _ (Program stmts) = dump stmts

instance StringDump Stmt where
  dump' indent stmt =
    case stmt of
      ExecStmt _ label execStmt -> dumpLabel label ++ dump' indent execStmt
      VarDef _ varIdent rexpr -> printf "auto %s%s;" varIdent (dumpAssignRexpr rexpr)
      ArrDef _ arrIdent size els -> printf "%s[%s]%s;" arrIdent (dumpArrSize size) (dumpElements els)
    where
      dumpLabel Nothing = ""
      dumpLabel (Just labelIdent) = labelIdent ++ ": "

      dumpAssignRexpr Nothing = ""
      dumpAssignRexpr (Just v) = printf " = %s" (dump v)

      dumpArrSize Nothing = ""
      dumpArrSize (Just size) = show size

      dumpElements els@(_ : _) = " " ++ intercalate ", " (map show els)
      dumpElements [] = ""

instance StringDump ExecStmt where
  dump' _ (Assign _ l r) = printf "%s = %s;" (dump l) (dump r)
  dump' indent (If _ cond then_ else_) =
    printf "if(%s) {\n%s\n}" (dump cond) (dump' (indent ++ "  ") then_) ++ dumpElse else_
    where
      dumpElse :: [Stmt] -> String
      dumpElse [] = ""
      dumpElse e = printf " else {\n%s\n}" $ dump' (indent ++ "  ") e
  dump' _ (Goto _ labelIdent) = printf "goto %s;" labelIdent

instance StringDump [Stmt] where
  dump' indent stmts = intercalate "\n" (map ((indent ++) . dump) stmts)

instance StringDump RightExpr where
  dump' _ (Literal _ int) = show int
  dump' _ (BinOpApp _ r op l) = printf "(%s %s %s)" (dump r) dumpOp (dump l)
    where
      dumpOp = case op of
        Add -> "+"
        Sub -> "-"
        BitOr -> "|"
        BitAnd -> "&"
        Eq -> "=="
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

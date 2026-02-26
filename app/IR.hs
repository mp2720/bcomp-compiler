{-# LANGUAGE NamedFieldPuns #-}

module IR where

import qualified AST as A
import Data.List (intercalate)
import Text.Printf (printf)

data FlatIdent
  = FlatIdent
      { intId :: Int,
        origId :: Maybe A.Ident
      }
  | -- | This should never appear in successfully emitted IR
    BogusIdent A.Ident

data Operand = Var FlatIdent | Const Integer | Address FlatIdent

newtype Label = Label FlatIdent

data SeqInstr
  = -- | dst = *src
    Load FlatIdent Operand
  | -- | dst = src
    Copy FlatIdent Operand
  | -- | *dst = src
    Store Operand Operand
  | -- | dst = opnd1 {+ - | &} opnd2
    BinOp FlatIdent Operand BinOp Operand
  | -- | dst = -src
    Negate FlatIdent Operand
  | -- | dst = ~src
    BitNot FlatIdent Operand

data BinOp
  = Add
  | Sub
  | BitOr
  | BitAnd

-- TODO: This representation is a good starting point, but it could be bad for
-- TODO: data flow analysis of conditions.
data BranchInstr
  = Branch Label
  | BranchIfZero Operand Label Label
  | BranchIfEq Operand Operand Label Label
  | BranchIfLt Operand Operand Label Label
  | BranchIfGe Operand Operand Label Label
  | BranchIfUnsignedLt Operand Operand Label Label
  | BranchIfUnsignedGe Operand Operand Label Label

-- | Linear IR
data LinearInstr
  = PlaceLabel Label
  | LBranchInstr BranchInstr
  | LSeqInstr SeqInstr

data VarKind
  = Scalar
  | Array [Integer]
  | -- | This should never appear in successfully emitted IR
    BogusKind

data LinearProgram = LinearProgram
  { progInstrs :: [LinearInstr],
    progVars :: [(FlatIdent, VarKind)],
    progLabels :: [FlatIdent]
  }

instance Show FlatIdent where
  show (FlatIdent intId Nothing) = show intId
  show (FlatIdent intId (Just origId)) = printf "%d[%s]" intId origId
  show (BogusIdent origId) = printf "UNRESOLVED[%s]" origId

instance Show Operand where
  show (Var ident) = printf "%%%s" (show ident)
  show (Const int) = printf "%d" int
  show (Address ident) = printf "$%s" (show ident)

instance Show Label where
  show (Label ident) = printf ":%s" (show ident)

instance Show SeqInstr where
  show (Load dst src) = printf "%%%s = ld %s" (show dst) (show src)
  show (Copy dst src) = printf "%%%s = %s" (show dst) (show src)
  show (Store dst src) = printf "st %s %s" (show dst) (show src)
  show (BinOp dst opnd1 op opnd2) = printf "%%%s = %s %s %s" (show dst) (show opnd1) (show op) (show opnd2)
  show (Negate dst src) = printf "%%%s = neg %s" (show dst) (show src)
  show (BitNot dst src) = printf "%%%s = not %s" (show dst) (show src)

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show BitOr = "|"
  show BitAnd = "&"

instance Show BranchInstr where
  show (Branch label) = printf "br %s" (show label)
  show (BranchIfZero opnd then_ else_) = printf "br (%s == 0) %s else %s" (show opnd) (show then_) (show else_)
  show (BranchIfEq opnd1 opnd2 then_ else_) =
    printf
      "br (%s == %s) %s else %s"
      (show opnd1)
      (show opnd2)
      (show then_)
      (show else_)
  show (BranchIfLt opnd1 opnd2 then_ else_) =
    printf
      "br (%s < %s) %s else %s"
      (show opnd1)
      (show opnd2)
      (show then_)
      (show else_)
  show (BranchIfGe opnd1 opnd2 then_ else_) =
    printf
      "br (%s >= %s) %s else %s"
      (show opnd1)
      (show opnd2)
      (show then_)
      (show else_)
  show (BranchIfUnsignedLt opnd1 opnd2 then_ else_) =
    printf "br (%s ^< %s) else %s" (show opnd1) (show opnd2) (show then_) (show else_)
  show (BranchIfUnsignedGe opnd1 opnd2 then_ else_) =
    printf "br (%s ^>= %s) %s else %s" (show opnd1) (show opnd2) (show then_) (show else_)

instance Show LinearInstr where
  show (PlaceLabel (Label label)) = show label ++ ":"
  show (LBranchInstr instr) = "    " ++ show instr
  show (LSeqInstr instr) = "    " ++ show instr

instance Show LinearProgram where
  show (LinearProgram {progInstrs, progVars}) = printf "VARS\n%s\nCODE\n%s" varsString codeString
    where
      varsString = intercalate "\n" $ map (("    " ++) . showVar) progVars
      codeString = intercalate "\n" $ map show progInstrs
      showVar (ident, Scalar) = show ident
      showVar (ident, Array elems) = printf "%s {%s}" (show ident) (intercalate "," (map show elems))
      showVar (ident, BogusKind) = printf "%s BOGUS" (show ident)

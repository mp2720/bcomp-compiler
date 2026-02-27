{-# LANGUAGE NamedFieldPuns #-}

module IR where

import Data.List (intercalate)
import ID (FlatID, Symbols)
import Text.Printf (printf)

data Operand = Var FlatID | Const Integer | Address FlatID

newtype Label = Label FlatID

data SeqInstr
  = -- | dst = *src
    Load FlatID Operand
  | -- | dst = src
    Copy FlatID Operand
  | -- | *dst = src
    Store Operand Operand
  | -- | dst = opnd1 {+ - | &} opnd2
    BinOp FlatID Operand BinOp Operand
  | -- | dst = -src
    Negate FlatID Operand
  | -- | dst = ~src
    BitNot FlatID Operand

data BinOp
  = Add
  | Sub
  | BitOr
  | BitAnd

-- TODO: This representation is a good starting point, but it could be bad for
-- TODO: data flow analysis of conditions.
data BranchInstr
  = Br Label
  | BrIf BrCond Label Label

data BrCond
  = IfZero Operand
  | IfEq Operand Operand
  | IfLt Operand Operand
  | IfGe Operand Operand
  | IfUnsignedLt Operand Operand
  | IfUnsignedGe Operand Operand

data LinearInstr
  = PlaceLabel Label
  | LBranchInstr BranchInstr
  | LSeqInstr SeqInstr

data VarKind
  = Scalar
  | Array [Integer]
  | -- | This should never appear in successfully emitted IR
    BogusKind

-- | Linear IR, unless optimized for codegen (the very last step), should not have implicit fallthroughs.
-- Thus, any label is followed by branch instr, sequential instr, or the end of the program.
-- And each program should start with a label.
-- And each label, except the start one, is preceded by branch instruction.
data LinearProgram = LinearProgram
  { progInstrs :: [LinearInstr],
    progVars :: Symbols VarKind,
    progLabels :: Symbols ()
  }

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
  show (Br label) = printf "br %s" (show label)
  show (BrIf cond then_ else_) = printf "br %s %s else %s" (show cond) (show then_) (show else_)

instance Show BrCond where
  show (IfZero opnd) = printf "%s == 0" (show opnd)
  show (IfEq opnd1 opnd2) = printf "%s == %s" (show opnd1) (show opnd2)
  show (IfLt opnd1 opnd2) = printf "%s < %s" (show opnd1) (show opnd2)
  show (IfGe opnd1 opnd2) = printf "%s >= %s" (show opnd1) (show opnd2)
  show (IfUnsignedLt opnd1 opnd2) = printf "%s ^< %s" (show opnd1) (show opnd2)
  show (IfUnsignedGe opnd1 opnd2) = printf "%s ^>= %s" (show opnd1) (show opnd2)

instance Show LinearInstr where
  show (PlaceLabel (Label label)) = show label ++ ":"
  show (LBranchInstr instr) = "    " ++ show instr
  show (LSeqInstr instr) = "    " ++ show instr

instance Show VarKind where
  show Scalar = "word"
  show (Array els) = printf "[%s]" $ intercalate ", " $ map show els
  show BogusKind = "BOGUS"

instance Show LinearProgram where
  show (LinearProgram {progInstrs, progVars}) = printf "VARS\n%s\nCODE\n%s" varsString codeString
    where
      varsString = unlines $ map ("  " ++) $ lines (show progVars)
      codeString = intercalate "\n" $ map show progInstrs

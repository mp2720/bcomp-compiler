{-# LANGUAGE NamedFieldPuns #-}

module Opt.UnusedVars (eliminate) where

import CFG (Block (Block, blockCode, blockOut), Edge (Cond), GraphProgram (GraphProgram), progBlks, progVars)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import Diagnostics (Diagnostic (Warning))
import ID (FlatID (FlatID), Symbols (..), filterSymbols)
import qualified IR
import Text.Printf (printf)

eliminate :: GraphProgram -> ([Diagnostic], GraphProgram)
eliminate prog@(GraphProgram {progBlks, progVars}) =
  ( mapMaybe (diagn "is an unused variable") (Set.elems unusedVars)
      ++ mapMaybe (diagn "is referenced, but never used") (Set.elems writtenNeverReadVars),
    prog {progVars = filterSymbols (\ident _ -> Set.member ident usedVars) progVars}
  )
  where
    diagn :: String -> FlatID -> Maybe Diagnostic
    diagn msg d = case d of
      (FlatID _ Nothing) -> Nothing
      (FlatID _ (Just (origID, pos))) ->
        Just $ Warning pos $ printf "%s %s" origID msg

    unusedVars = allVars \\ usedVars
    writtenNeverReadVars = usedVars \\ varReads
    allVars = Map.keysSet (symbols progVars)
    usedVars = Set.union varReads varWrites

    varUse :: (IR.SeqInstr -> [FlatID]) -> (IR.BrCond -> [FlatID]) -> Set FlatID
    varUse varUseSeqInstr varUseBrCond = Set.unions (map varUseBlk (Map.elems progBlks))
      where
        varUseBlk Block {blockCode, blockOut} =
          Set.fromList $
            asum (map varUseSeqInstr blockCode) ++ varUseEdge blockOut
          where
            varUseEdge (Cond cond _ _) = varUseBrCond cond
            varUseEdge _ = []

    varReads = varUse i (asum . map varUseOpnd . c)
      where
        i (IR.Load _ src) = varUseOpnd src
        i (IR.Copy _ src) = varUseOpnd src
        i (IR.Store _ src) = varUseOpnd src
        i (IR.BinOp _ l _ r) = varUseOpnd l ++ varUseOpnd r
        i (IR.Negate _ opnd) = varUseOpnd opnd
        i (IR.BitNot _ opnd) = varUseOpnd opnd

        c (IR.IfZero opnd) = [opnd]
        c (IR.IfEq l r) = [l, r]
        c (IR.IfLt l r) = [l, r]
        c (IR.IfGe l r) = [l, r]
        c (IR.IfUnsignedLt l r) = [l, r]
        c (IR.IfUnsignedGe l r) = [l, r]

    varWrites = varUse i c
      where
        i (IR.Load dst _) = [dst]
        i (IR.Copy dst _) = [dst]
        i (IR.Store dst _) = varUseOpnd dst
        i (IR.BinOp dst _ _ _) = [dst]
        i (IR.Negate dst _) = [dst]
        i (IR.BitNot dst _) = [dst]

        c _ = []

    varUseOpnd (IR.Var varID) = [varID]
    varUseOpnd (IR.Const _) = []
    varUseOpnd (IR.Address varID) = [varID]

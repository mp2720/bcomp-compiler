{-# LANGUAGE NamedFieldPuns #-}

module Opt.UnusedVars (eliminateUnusedVar) where

import CFG (Block, GraphProgram (GraphProgram), progBlks, progVars)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Diagnostics (Diagnostic (Warning))
import ID (FlatID (FlatID), Symbols (..), emptySymbols)
import qualified IR
import Text.Printf (printf)

data Pass = Pass
  { unusedVars :: Map Int (FlatID, IR.VarDecl),
    usedVars :: Symbols IR.VarDecl
  }

eliminateUnusedVar :: GraphProgram -> ([Diagnostic], GraphProgram)
eliminateUnusedVar prog@(GraphProgram {progBlks, progVars}) =
  ( mapMaybe diagn (Map.elems $ unusedVars pass),
    prog
      { progBlks = graph,
        progVars = usedVars pass
      }
  )
  where
    diagn (FlatID _ (Just origID), IR.VarDecl pos _) =
      Just $ Warning pos $ printf "unused variable %s" origID
    diagn _ = Nothing

    (pass, graph) = Map.mapAccum f (Pass (symbols progVars) emptySymbols) progBlks

    f :: Pass -> Block -> (Pass, Block)
    f pass blk = undefined

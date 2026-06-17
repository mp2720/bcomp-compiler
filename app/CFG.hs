module CFG where

import Data.Map (Map)
import ID (FlatID, Symbols)
import qualified IR

data GraphProgram = GraphProgram
  { progBlks :: Graph,
    progStartBlkID :: FlatID,
    progVars :: Symbols IR.VarKind,
    progLabels :: Symbols ()
  }

-- TODO: maybe introduce distinct types for "code" IDs and var IDs?

data Block = Block
  { blockFlatID :: FlatID,
    blockCode :: [IR.SeqInstr],
    blockPreds :: [FlatID],
    blockOut :: Edge
  }

data Edge = Uncond FlatID | Cond IR.BrCond FlatID FlatID | Sink

type Graph = Map FlatID Block

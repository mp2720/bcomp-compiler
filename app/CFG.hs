module CFG where

import Data.Map (Map)
import ID (FlatID, Symbols)
import qualified IR

data GraphProgram = GraphProgram
  { progBlks :: Graph,
    progVars :: Symbols IR.VarDecl,
    progLabels :: Symbols ()
  }

-- | Block has the same int ID as its label, if it has the one associated with it.
-- Otherwise the block is called synthetic and has a synthetic id.
-- Id is used as key in graph.
type BlockID = Int

data Block = Block
  { blockFlatID :: FlatID,
    blockCode :: [IR.SeqInstr],
    blockPreds :: [BlockID],
    blockOut :: Edge
  }

data Edge = Uncond BlockID | Cond IR.BrCond BlockID BlockID | Sink

type Graph = Map Int Block

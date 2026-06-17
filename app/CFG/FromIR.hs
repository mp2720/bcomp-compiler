{-# LANGUAGE TemplateHaskell #-}

module CFG.FromIR (convert) where

import CFG
import Control.Lens.Basic (field, over)
import Control.Monad (forM_)
import Control.Monad.Trans.State (State, evalState, get, modify, state)
import qualified Data.Map as Map
import ID (FlatID (..), Symbols, newSymbol)
import qualified IR

data Conv = Conv
  { -- | instructions are reversed
    curBlk :: Maybe (FlatID, [IR.SeqInstr]),
    graph :: Graph,
    labels :: Symbols ()
  }

convert :: IR.LinearProgram -> GraphProgram
convert (IR.LinearProgram instrs vars linLabels) =
  evalState
    ( do
        startBlkID <- state newSyntheticBlk
        modifyCurBlk $ const (Just (startBlkID, []))

        convInstr instrs

        -- Push the last block
        conv <- get
        case curBlk conv of
          Nothing -> return ()
          Just (lastBlkID, lastBlkCode) ->
            pushBlock $
              Block
                { blockFlatID = lastBlkID,
                  blockCode = reverse lastBlkCode,
                  blockPreds = [],
                  blockOut = Sink
                }

        updConv <- get
        return $
          GraphProgram
            { progBlks = computePreds $ graph updConv,
              progStartBlkID = startBlkID, -- Always points to an existing block.
              progVars = vars,
              progLabels = labels updConv
            }
    )
    Conv {curBlk = Nothing, graph = Map.empty, labels = linLabels}

computePreds :: Graph -> Graph
computePreds og = Map.foldr predsForBlk og og
  where
    predsForBlk blk g = case blockOut blk of
      Uncond to -> addPred to g
      Cond _ then_ else_ -> addPred then_ $ addPred else_ g
      Sink -> g
      where
        addPred = Map.adjust (over $(field 'blockPreds) (blkID :))
        blkID = intID $ blockFlatID blk

-- The following algorithm is best thought of as a finite automaton that iterates through
-- instructions and stores some state, including the current block label and its instructions.
-- At some point, the state may not point to any block, necessitating the creation of a
-- new synthetic block.
--
-- Labels and branches cause the automaton to push the current block to the graph.
--
-- The automaton begins with a block that has "start" label.
--
-- + When encountering a label, it pushes the previous block (if there's one) and creates a new one.
-- + On a seq instruction, it collects the instr to the current block
--  (or to the new one it created, if there is none).
-- + On a branch instruction, it pushes the current block (creating a new one if there is none)
--   to the graph.
--
-- Finally, after finishing, it pushes the last block (if there's one).

convInstr :: [IR.LinearInstr] -> State Conv ()
convInstr instrs = forM_ instrs go
  where
    go (IR.PlaceLabel (IR.Label label)) = do
      conv <- get
      case curBlk conv of
        Nothing -> return ()
        Just (prevBlkID, prevBlkCode) ->
          -- If there was a block, push it to the graph
          pushBlock $
            Block
              { blockFlatID = prevBlkID,
                blockCode = reverse prevBlkCode,
                blockPreds = [],
                blockOut = Uncond (intID label)
              }
      -- start a new block
      modifyCurBlk $ const (Just (label, []))
    go (IR.LBranchInstr br) = do
      conv <- get

      -- compute outgoing arrow
      let blkOut = case br of
            IR.Br (IR.Label label) -> Uncond $ intID label
            IR.BrIf cond (IR.Label then_) (IR.Label else_) ->
              Cond cond (intID then_) (intID else_)

      blk <- case curBlk conv of
        Just (blkID, blkCode) ->
          return $
            Block
              { blockFlatID = blkID,
                blockCode = reverse blkCode,
                blockPreds = [],
                blockOut = blkOut
              }
        Nothing -> do
          -- no current block, create new
          blkID <- state newSyntheticBlk
          return $
            Block
              { blockFlatID = blkID,
                blockCode = [],
                blockPreds = [],
                blockOut = blkOut
              }

      -- push to the graph
      pushBlock blk

      -- we're done with this block
      modifyCurBlk $ const Nothing
    go (IR.LSeqInstr seq_) = do
      conv <- get
      case curBlk conv of
        Just (blkID, blkCode) ->
          modifyCurBlk $ const (Just (blkID, seq_ : blkCode))
        Nothing -> do
          blkID <- state newSyntheticBlk
          modifyCurBlk $ const (Just (blkID, [seq_]))

pushBlock :: Block -> State Conv ()
pushBlock blk =
  modify $
    over
      $(field 'graph)
      ( Map.insert (intID $ blockFlatID blk) blk
      )

modifyCurBlk ::
  ( Maybe (FlatID, [IR.SeqInstr]) ->
    Maybe (FlatID, [IR.SeqInstr])
  ) ->
  State Conv ()
modifyCurBlk = modify . over $(field 'curBlk)

newSyntheticBlk :: Conv -> (FlatID, Conv)
newSyntheticBlk conv = (flatId, conv {labels = updLabels})
  where
    (flatId, updLabels) = newSymbol Nothing () $ labels conv

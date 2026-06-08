{-# LANGUAGE NamedFieldPuns #-}

module ID
  ( FlatID (..),
    bogusID,
    Symbols (Symbols, symbols),
    emptySymbols,
    newSymbol,
  )
where

import qualified AST as A
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

data FlatID
  = FlatID
  { intID :: Int,
    origID :: Maybe A.Ident
  }

instance Show FlatID where
  show (FlatID intId Nothing) = show intId
  show (FlatID intId (Just origId)) = printf "%d{%s}" intId origId

-- | No IR should ever contain this after successful pass.
bogusID :: FlatID
bogusID = FlatID (-1) (Just "BOGUS")

data Symbols s = Symbols
  { symbols :: Map Int (FlatID, s),
    uniqueCnt :: Int
  }

emptySymbols :: Symbols s
emptySymbols = Symbols Map.empty 0

newSymbol :: Maybe A.Ident -> s -> Symbols s -> (FlatID, Symbols s)
newSymbol astID s Symbols {symbols, uniqueCnt} =
  ( flatID,
    Symbols
      { symbols = Map.insert (intID flatID) (flatID, s) symbols,
        uniqueCnt = uniqueCnt + 1
      }
  )
  where
    flatID = FlatID uniqueCnt astID

instance (Show s) => Show (Symbols s) where
  show (Symbols {symbols}) = intercalate "\n" $ map showSym $ Map.elems symbols
    where
      showSym (flatID, s) = printf "%s %s" (show flatID) (show s)

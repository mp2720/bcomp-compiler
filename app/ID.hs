{-# LANGUAGE NamedFieldPuns #-}

module ID
  ( FlatID (..),
    bogusID,
    Symbols (Symbols, symbols),
    emptySymbols,
    newSymbol,
    filterSymbols,
  )
where

import qualified AST as A
import Data.Function (on)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

-- | A new FlatID should be constructed only by using the 'newSymbol'.
data FlatID
  = FlatID
  { intID :: Int,
    origID :: Maybe (A.Ident, Maybe A.P) -- Stored here for diagnostics purpose
  }

instance Eq FlatID where
  (==) = on (==) intID

instance Ord FlatID where
  compare = on compare intID

instance Show FlatID where
  show (FlatID intId Nothing) = show intId
  show (FlatID intID (Just (origID, Nothing))) = printf "%d{%s}" intID origID
  show (FlatID intID (Just (origID, Just origPos))) = printf "%d{%s at %s}" intID origID (show origPos)

-- | No IR should ever contain this after successful pass.
bogusID :: FlatID
bogusID = FlatID (-1) (Just ("BOGUS", Nothing))

data Symbols s = Symbols
  { symbols :: Map FlatID s,
    uniqueCnt :: Int
  }

emptySymbols :: Symbols s
emptySymbols = Symbols Map.empty 0

newSymbol :: Maybe (A.Ident, Maybe A.P) -> s -> Symbols s -> (FlatID, Symbols s)
newSymbol origID s Symbols {symbols, uniqueCnt} =
  ( flatID,
    Symbols
      { symbols = Map.insert flatID s symbols,
        uniqueCnt = uniqueCnt + 1
      }
  )
  where
    flatID = FlatID uniqueCnt origID

filterSymbols :: (FlatID -> s -> Bool) -> Symbols s -> Symbols s
filterSymbols p Symbols {symbols, uniqueCnt} =
  Symbols (Map.filterWithKey p symbols) uniqueCnt

instance (Show s) => Show (Symbols s) where
  show (Symbols {symbols}) = intercalate "\n" $ map showSym $ Map.toList symbols
    where
      showSym (flatID, s) = printf "%s %s" (show flatID) (show s)

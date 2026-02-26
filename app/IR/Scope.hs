{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module IR.Scope
  ( Scope,
    empty,
    lookupSymbol,
    lookupSymbolRec,
    declSymbol,
    startChild,
    endChild,
  )
where

import AST qualified as A
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Map qualified as Map
import IR (FlatIdent (..))

data Scope s
  = Scope
  { symbols :: Map A.Ident (FlatIdent, s),
    parent :: Maybe (Scope s),
    uniqueCnt :: Int
  }

empty :: Scope s
empty =
  Scope
    { symbols = Map.empty,
      parent = Nothing,
      uniqueCnt = 0
    }

lookupSymbol' :: (Scope s -> A.Ident -> Maybe (FlatIdent, s)) -> Scope s -> A.Ident -> Maybe (FlatIdent, s)
lookupSymbol' rec Scope {symbols, parent} ident =
  Map.lookup ident symbols
    <|> ((`rec` ident) =<< parent) -- crazy LSP suggestion (run `rec` if parent is present)

-- | Lookup in the current scope.
lookupSymbol :: Scope s -> A.Ident -> Maybe (FlatIdent, s)
lookupSymbol = lookupSymbol' (const2 Nothing)
  where
    const2 a _ _ = a

-- | Lookup recursively.
lookupSymbolRec :: Scope s -> A.Ident -> Maybe (FlatIdent, s)
lookupSymbolRec = lookupSymbol' lookupSymbolRec

declSymbol :: Scope s -> Maybe A.Ident -> s -> (FlatIdent, Scope s)
declSymbol scope@Scope {symbols, uniqueCnt} mbIdent s =
  ( flatId,
    scope
      { symbols = maybe symbols (\ident -> Map.insert ident (flatId, s) symbols) mbIdent,
        uniqueCnt = uniqueCnt + 1
      }
  )
  where
    flatId = FlatIdent uniqueCnt mbIdent

startChild :: Scope s -> Scope s
startChild parent@Scope {uniqueCnt = pUniqueCnt} =
  empty
    { uniqueCnt = pUniqueCnt,
      parent = Just parent
    }

endChild :: Scope s -> Scope s -> Scope s
endChild parent _child@Scope {uniqueCnt = chUniqueCnt} = parent {uniqueCnt = chUniqueCnt}

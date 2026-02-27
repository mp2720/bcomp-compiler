{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module IR.Scope
  ( Scope,
    empty,
    lookupSymbol,
    lookupSymbolRec,
    addSymbol,
    mkChild,
  )
where

import AST qualified as A
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Map qualified as Map
import ID (FlatID (..))

data Scope s
  = Scope
  { symbolsMap :: Map A.Ident (FlatID, s),
    parent :: Maybe (Scope s)
  }

empty :: Scope s
empty =
  Scope
    { symbolsMap = Map.empty,
      parent = Nothing
    }

lookupSymbol' ::
  (A.Ident -> Scope s -> Maybe (FlatID, s)) ->
  A.Ident ->
  Scope s ->
  Maybe (FlatID, s)
lookupSymbol' rec ident Scope {symbolsMap, parent} =
  Map.lookup ident symbolsMap <|> (rec ident =<< parent)

-- | Lookup in the current scope.
lookupSymbol :: A.Ident -> Scope s -> Maybe (FlatID, s)
lookupSymbol = lookupSymbol' (const2 Nothing)
  where
    const2 a _ _ = a

-- | Lookup recursively.
lookupSymbolRec :: A.Ident -> Scope s -> Maybe (FlatID, s)
lookupSymbolRec = lookupSymbol' lookupSymbolRec

addSymbol :: A.Ident -> FlatID -> s -> Scope s -> Scope s
addSymbol astID flatID s scope@Scope {symbolsMap} =
  scope
    { symbolsMap = Map.insert astID (flatID, s) symbolsMap
    }

mkChild :: Scope s -> Scope s
mkChild parent = Scope {symbolsMap = Map.empty, parent = Just parent}

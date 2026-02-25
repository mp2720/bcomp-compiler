{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module IR.Scope
  ( Scope,
    empty,
    lookupSymbol,
    lookupSymbolRec,
    declSymbol,
    declSyntheticSymbol,
    mkChild,
  )
where

import AST qualified as A
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Map qualified as Map
import IR (FlatIdent (..))
import Text.Printf (printf)

data Scope s
  = Scope
  { scopeId :: FlatIdent,
    symbols :: Map A.Ident (FlatIdent, s),
    parent :: Maybe (Scope s),
    uniqueCnt :: Integer
  }

empty :: Scope s
empty = Scope {scopeId = FlatIdent "", symbols = Map.empty, parent = Nothing, uniqueCnt = 0}

mkFlatIdent :: Scope s -> String -> FlatIdent
mkFlatIdent Scope {parent = Nothing} ident = FlatIdent ident
mkFlatIdent Scope {parent = Just _, scopeId} ident = FlatIdent $ printf "%s/%s" (show scopeId) ident

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

declSymbol :: Scope s -> A.Ident -> s -> (FlatIdent, Scope s)
declSymbol scope@Scope {symbols} ident s =
  (flatIdent, scope {symbols = Map.insert ident (flatIdent, s) symbols})
  where
    flatIdent = mkFlatIdent scope ident

declSyntheticSymbol :: Scope s -> s -> (FlatIdent, Scope s)
declSyntheticSymbol scope@Scope {uniqueCnt} =
  declSymbol scope {uniqueCnt = uniqueCnt + 1} (show uniqueCnt)

mkChild ::
  Scope s ->
  -- | Pair of (child, parent)
  (Scope s, Scope s)
mkChild parent@Scope {uniqueCnt = parentCnt} =
  ( Scope
      { scopeId = childFlatId,
        symbols = Map.empty,
        parent = Just parent,
        uniqueCnt = 0
      },
    parentUpd
  )
  where
    childFlatId = mkFlatIdent parent (show parentCnt)
    parentUpd = parent {uniqueCnt = parentCnt + 1}

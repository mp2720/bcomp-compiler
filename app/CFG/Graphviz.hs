{-# LANGUAGE NamedFieldPuns #-}

module CFG.Graphviz (dump) where

import CFG
import Control.Applicative (asum)
import Data.List (intercalate)
import qualified Data.Map as Map
import ID
import qualified IR
import Text.Printf (printf)

-- Style is based on LLVM CFG
-- Dark theme: https://cprimozic.net/notes/posts/basic-graphviz-dark-theme-config

dump :: GraphProgram -> String
dump GraphProgram {progBlks, progStartBlkID, progVars} =
  printf "digraph {\n%s%s%s\n}" darkTheme vars (intercalate "\n" $ map gBlk $ Map.elems progBlks)
  where
    vars :: String
    vars = printf "vars[shape=record,style=dashed,fontname=\"Courier\",label=\"%s\"];\n" (intercalate "\\l" $ map (escRec . var) $ symbols progVars)

    var (ident, IR.Scalar) = show ident
    var (ident, IR.Array els) = printf "%s[%d]" (show ident) (length els)
    var (ident, IR.BogusKind) = printf "%s BOGUS" (show ident)

    gBlk Block {blockFlatID, blockCode, blockOut} =
      node ++ edges
      where
        node :: String
        node =
          if blockFlatID == progStartBlkID
            then
              printf
                "%s[shape=record,fontname=\"Courier\",label=\"%s\",fillcolor=\"%s\"];\n"
                (show $ intID blockFlatID)
                nodeLabel
                darkThemeStartBlkFillColor
            else
              printf
                "%s[shape=record,fontname=\"Courier\",label=\"%s\"];\n"
                (show $ intID blockFlatID)
                nodeLabel

        nodeLabel :: String
        nodeLabel =
          printf
            "{%s:\\l|%s\\l%s}"
            (escRec $ show blockFlatID)
            (intercalate "\\l" $ map (escRec . show) blockCode)
            branch

        branch = case blockOut of
          Uncond _ -> "br\\l"
          Cond br _ _ -> printf "br %s\\l|{<t>T|<f>F}" (escRec $ show br)
          Sink -> "SINK"

        edges = case blockOut of
          Uncond toID -> printf "%d -> %d;" (intID blockFlatID) toID
          Cond _ thenID elseID ->
            printf
              "%d:t -> %d; %d:f -> %d;"
              (intID blockFlatID)
              thenID
              (intID blockFlatID)
              elseID
          Sink -> ""
    darkTheme =
      asum
        [ "bgcolor=\"#181818\";",
          "node [",
          " fontcolor = \"#e6e6e6\",",
          " style = filled,",
          " color = \"#e6e6e6\",",
          " fillcolor = \"#333333\"",
          "]\n",
          " edge [",
          " color = \"#e6e6e6\",",
          " fontcolor = \"#e6e6e6\"",
          "]\n"
        ]
    darkThemeStartBlkFillColor = "#235a87"

-- | Escape record text
-- https://graphviz.org/doc/info/shapes.html#record
escRec :: String -> String
escRec = go
  where
    go (c : cs) = case c of
      '|' -> "\\|" ++ go cs
      '{' -> "\\{" ++ go cs
      '}' -> "\\}" ++ go cs
      '<' -> "\\<" ++ go cs
      '>' -> "\\>" ++ go cs
      '"' -> "\\\"" ++ go cs
      '\\' -> "\\\\" ++ go cs
      _ -> c : go cs
    go "" = ""

module Diagnostics
  ( P (..),
    Diagnostic (..),
    haveErrors,
  )
where

import Text.Printf (printf)

data P = Position
  { positionOffset :: Int,
    positionLine :: Int,
    positionColumn :: Int
  }
  deriving (Eq)

instance Show P where
  show (Position _ line column) = printf "%d:%d" line column

data Diagnostic = Error P String | Warning P String

haveErrors :: [Diagnostic] -> Bool
haveErrors = any isError
  where
    isError (Error _ _) = True
    isError (Warning _ _) = False

instance Show Diagnostic where
  show (Error pos msg) = printf "error at %s: %s" (show pos) msg
  show (Warning pos msg) = printf "warning at %s: %s" (show pos) msg

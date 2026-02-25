module Diagnostics (Diagnostic (..), haveErrors) where

import qualified AST as A
import Text.Printf (printf)

data Diagnostic = Error A.P String | Warning A.P String

haveErrors :: [Diagnostic] -> Bool
haveErrors = any isError
  where
    isError (Error _ _) = True
    isError (Warning _ _) = False

instance Show Diagnostic where
  show (Error pos msg) = printf "error at %s: %s" (show pos) msg
  show (Warning pos msg) = printf "warning at %s: %s" (show pos) msg

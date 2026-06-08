module Diagnostics (Diagnostic (..), haveErrors) where

import qualified AST as A
import Text.Printf (printf)

data Diagnostic = Error (Maybe A.P) String | Warning (Maybe A.P) String

haveErrors :: [Diagnostic] -> Bool
haveErrors = any isError
  where
    isError (Error _ _) = True
    isError (Warning _ _) = False

instance Show Diagnostic where
  show msg = case msg of
    (Error pos text) -> printf "error%s: %s" (showPos pos) text
    (Warning pos text) -> printf "warning%s: %s" (showPos pos) text
    where
      showPos (Just pos) = printf " at %s" (show pos)
      showPos Nothing = ""

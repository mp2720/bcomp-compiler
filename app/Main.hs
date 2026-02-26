module Main where

import AST (StringDump (dump))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (runExceptT), except, throwE)
import Data.Bifunctor (first)
import Data.List (intercalate)
import IR.FromAST (convert)
import Parse.Combinators (Parser (runParser), eof)
import Parse.Grammar (program)
import Parse.Lexer (lexerPosition, regularLexerState)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

compile :: ExceptT String IO ()
compile = do
  args <- liftIO getArgs
  source <- case args of
    [sourcePath] -> liftIO $ readFile sourcePath
    _ -> throwE "invalid usage"

  (ast, _) <-
    except $
      first
        (printf "syntax error at %s" . show . lexerPosition)
        (runParser (program <* eof) (regularLexerState source))

  liftIO $ createDirectoryIfMissing True "output"

  -- TODO: make output path configurable

  liftIO $ writeFile "output/prog.ast" (dump ast)

  prog <-
    except $
      first
        (intercalate "\n" . map show)
        (convert ast)

  liftIO $ writeFile "output/prog.ir" (show prog)

main :: IO ()
main = do
  result <- runExceptT compile
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right () -> pure ()

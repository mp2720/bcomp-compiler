module Main (main) where

import qualified AST
import qualified CFG.FromIR
import qualified CFG.Graphviz
import qualified Control.Exception as Exception
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (runExceptT), throwE)
import Diagnostics
import qualified IR.FromAST
import Parse.Grammar (parseProgram)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

ppToFile :: String -> (a -> String) -> a -> IO ()
ppToFile filename pp a = do
  let s = pp a
  hPutStrLn stderr (printf "writing %s" filename)
  writeFile filename s

passDiagn ::
  (a -> ([Diagnostic], b)) ->
  (b -> IO ()) ->
  a ->
  ExceptT () IO b
passDiagn p pp a = do
  let (diagns, b) = p a
  liftIO $ forM_ diagns print
  liftIO $ pp b
  if haveErrors diagns
    then throwE ()
    else pure b

pass ::
  (a -> b) ->
  (b -> IO ()) ->
  a ->
  ExceptT () IO b
pass p = passDiagn ((,) [] . p)

rmDirIfExists :: FilePath -> IO ()
rmDirIfExists path = Exception.handle handler $ removeDirectoryRecursive path
  where
    handler :: Exception.IOException -> IO ()
    handler e
      | isDoesNotExistError e = pure ()
      | otherwise = Exception.throwIO e

compile :: ExceptT () IO ()
compile = do
  args <- liftIO getArgs
  source <- case args of
    [sourcePath] -> liftIO $ readFile sourcePath
    _ -> do
      liftIO $ hPutStrLn stderr "invalid usage"
      throwE ()

  liftIO $ rmDirIfExists outputDir
  liftIO $ createDirectoryIfMissing True outputDir

  _ <-
    -- Passes
    passDiagn parseProgram (pp "00_ast.b" AST.dump) source
      >>= passDiagn IR.FromAST.convert (pp "10_lin.ir" show)
      >>= pass CFG.FromIR.convert (pp "20_cfg.dot" CFG.Graphviz.dump)

  return ()
  where
    pp filename = ppToFile $ outputDir ++ "/" ++ filename
    -- TODO: make output path configurable
    outputDir = "output"

main :: IO ()
main = do
  result <- runExceptT compile
  case result of
    Left () -> exitWith (ExitFailure 1)
    Right () -> pure ()

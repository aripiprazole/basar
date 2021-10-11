module Basar.Cli where

import Basar.Codegen.Compiler (codegen, compile)
import Basar.Parsing.Parser (parseBasar)
import Basar.Typechecking.Typecheck (InferError, typecheck)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (evalState)
import GHC.IO (liftIO)
import qualified LLVM.AST as AST
import Text.Megaparsec.Error (errorBundlePretty)

pipeline :: ExceptT String IO ()
pipeline = do
  content <- lift $ readFile "./examples/main.bs"

  ast <- case parseBasar content of
    Right ast -> pure ast
    Left err -> throwE $ errorBundlePretty err

  ast' <- case typecheck ast of
    Right ast -> pure ast
    Left err -> throwE $ show err

  lift $ putStrLn "<=============>"
  lift $ putStrLn $ "AST         => " ++ show ast
  lift $ putStrLn $ "TY AST      => " ++ show ast'
  lift $ putStrLn "<=============>"

  compile ast'

  return ()

runCli :: IO ()
runCli = do
  result <- runExceptT pipeline

  case result of
    Right () -> return ()
    Left err -> print err
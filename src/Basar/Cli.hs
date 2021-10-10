module Basar.Cli where

import Basar.Parsing.Parser (parseBasar)
import Basar.Typechecking.Env (defaultEnv)
import Basar.Typechecking.Typecheck (typecheck)
import Text.Megaparsec.Error (errorBundlePretty)

runCli :: IO ()
runCli = do
  content <- readFile "./examples/main.bs"

  case parseBasar content of
    Right program -> print $ typecheck program defaultEnv
    Left l -> putStrLn $ errorBundlePretty l

  return ()

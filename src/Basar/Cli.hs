module Basar.Cli where

import Basar.Parsing.Parser (parseBasar)
import Text.Megaparsec.Error (errorBundlePretty)

runCli :: IO ()
runCli = do
  content <- readFile "./examples/main.bs"

  case parseBasar content of
    Right r -> print r
    Left l -> putStrLn $ errorBundlePretty l

  return ()

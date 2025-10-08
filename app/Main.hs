module Main (main) where

import Emit
import Parse
import System.Environment
import Translators.Common
import Translators.Python

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then
      putStrLn "please supply one (1) argument"
    else do
      parseResult <- parseFile (head args)
      case parseResult of
        Right ast -> putStrLn <$> translate translator $ emit ast
        Left err -> print err

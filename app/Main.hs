module Main (main) where

import Data.List (intercalate)
import Data.Map.Strict (Map, assocs, fromList, (!?))
import Lex (lexer)
import Parse (parser)
import System.Environment (getArgs)

whoops :: String -> IO ()
whoops message = putStrLn $ "[error] " ++ message

unreachable :: IO ()
unreachable = do
  whoops "internal compiler error: unreachable case reached"
  error "unreachable"

data Subcommand = Subcommand
  { action :: [String] -> IO (),
    argc :: [Int],
    description :: String,
    longHelp :: String
  }

run :: String -> [String] -> IO ()
run command args = mapSub run' command
  where
    run' a =
      if length args `elem` argc a
        then action a args
        else do
          whoops "incorrect argument count"
          help [command]

subcommands :: Map String Subcommand
subcommands =
  fromList
    [ ( "help",
        Subcommand
          { action = help,
            argc = [0, 1],
            description = "show this help message",
            longHelp = "with no arguments, it will list available subcommands. supply a subcommand and it will print help for that subcommand."
          }
      ),
      ( "lex",
        Subcommand
          { action = lex_,
            argc = [1],
            description = "lex a file",
            longHelp = "pass a file path, and the tokens in the file will be printed."
          }
      ),
      ( "parse",
        Subcommand
          { action = parse,
            argc = [1],
            description = "parse a file",
            longHelp = "pass a file path, and the abstract syntax tree of the file will be printed."
          }
      )
    ]

mapSub :: (Subcommand -> IO ()) -> String -> IO ()
mapSub f subcommand = case subcommands !? subcommand of
  Just a -> f a
  Nothing -> do
    whoops $ "no such subcommand: " ++ subcommand
    help []

help :: [String] -> IO ()
help [] = do
  putStrLn "== available subcommands =="
  putStrLn $ intercalate "\n" $ map fmt (assocs subcommands)
  where
    fmt (name, subcommand) = name ++ " :: " ++ description subcommand
help [subcommand] =
  mapSub
    ( \a ->
        do
          putStrLn $ "== help for command `" ++ subcommand ++ "` =="
          putStrLn $ "can take " ++ show (argc a) ++ " arguments"
          putStrLn $ longHelp a
    )
    subcommand
help _ = unreachable

lex_ :: [String] -> IO ()
lex_ [file] = do
  content <- readFile file
  case lexer content of
    Right tokens -> putStrLn $ intercalate "\n" $ map show tokens
    Left err -> whoops $ "lexing error: " ++ show err
lex_ _ = unreachable

parse :: [String] -> IO ()
parse [file] = do
  content <- readFile file
  case lexer content of
    Right tokens -> case parser tokens of
      Right ast -> print ast
      Left err -> whoops $ "parsing error: " ++ show err
    Left err -> whoops $ "lexing error: " ++ show err
parse _ = unreachable

main :: IO ()
main = do
  args <- getArgs
  case args of
    (subcommand : rest) -> run subcommand rest
    _ -> do
      whoops "a subcommand is required"
      help []

{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parse
  ( AST (..),
    AtomT (..),
    LiteralT (..),
    SExprT (..),
    parseAST,
    parseFile,
  )
where

import Text.ParserCombinators.Parsec

data AST where
  ASTSExprs :: [SExprT] -> AST
  deriving (Show)

data AtomT where
  AtomID :: String -> AtomT
  deriving (Show)

data LiteralT = LiteralAtom AtomT | LiteralInteger Integer | LiteralString String deriving (Show)

data SExprT
  = SExprLiteral LiteralT
  | SExprInner
      { sop :: AtomT,
        sargs :: [SExprT]
      }
  deriving (Show)

ast :: Parser AST
ast = do
  exprs <- many (spaces *> sexpr <* spaces)
  eof
  return $ ASTSExprs exprs

atom :: Parser AtomT
atom = do
  inner <- many (letter <|> oneOf "!@#$%^&*+-=_")
  return $ AtomID inner

literal :: Parser LiteralT
literal =
  (LiteralString <$> str)
    <|> (LiteralInteger <$> int)
    <|> (LiteralAtom <$> atom)
  where
    int = read <$> many1 digit
    str = char '"' *> many (noneOf "\"") <* char '"'

sexpr :: Parser SExprT
sexpr = do
  char '('
  spaces
  op <- atom
  args <- many $ many1 space *> (sexpr <|> SExprLiteral <$> literal)
  spaces
  char ')'
  return $ SExprInner {sop = op, sargs = args}

parseAST :: String -> String -> Either ParseError AST
parseAST = parse ast

parseFile :: String -> IO (Either ParseError AST)
parseFile fName = parseAST fName <$> readFile fName

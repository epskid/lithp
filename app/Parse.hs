module Parse (parser) where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Lex (Token)
import qualified Lex as Token (Token (..))

newtype AST = AST [Expr] deriving (Show)

data Expr
  = Normal NormalExpr
  | Quoted QuotedExpr
  deriving (Show)

data NormalExpr = NormalExpr HeadAtom [Atom] deriving (Show)

newtype QuotedExpr = QuotedExpr [Atom] deriving (Show)

data HeadAtom
  = ExprAtom' NormalExpr
  | Symbol' String
  deriving (Show)

data Atom
  = ExprAtom Expr
  | StringLiteral String
  | IntegerLiteral Integer
  | FloatLiteral Float
  | Symbol String
  deriving (Show)

newtype Parser a = Parser
  { runParser :: [Token] -> [(a, [Token])]
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure x = Parser $ \input -> [(x, input)]
  Parser pF <*> Parser pA = Parser $ \input -> do
    (f, rest) <- pF input
    (a, rest') <- pA rest
    return (f a, rest')

instance Alternative Parser where
  empty = Parser $ const []
  Parser pA <|> Parser pB = Parser $ \input -> pA input ++ pB input

satisfies :: (Token -> Bool) -> Parser Token
satisfies p = Parser $ \case
  (t : ts) | p t -> [(t, ts)]
  _ -> []

token :: Token -> Parser Token
token t = satisfies (== t)

munch :: (Token -> Maybe a) -> Parser a
munch p = Parser $ \case
  t : ts -> case p t of
    Just res -> [(res, ts)]
    _ -> []
  _ -> []

literal :: Parser Atom
literal = munch $
  \case
    Token.StringLiteral s -> Just $ StringLiteral s
    Token.IntegerLiteral i -> Just $ IntegerLiteral i
    Token.FloatLiteral f -> Just $ FloatLiteral f
    Token.Symbol s -> Just $ Symbol s
    _ -> Nothing

atom :: Parser Atom
atom =
  ExprAtom <$> expr <|> literal

headAtom :: Parser HeadAtom
headAtom =
  ExprAtom' <$> normalExpr <|> nonQuotedExpr
  where
    nonQuotedExpr = munch $ \case
      Token.Symbol s | s /= "quote" -> Just $ Symbol' s
      _ -> Nothing

normalExpr :: Parser NormalExpr
normalExpr = token Token.LParen *> (NormalExpr <$> headAtom <*> many atom) <* token Token.RParen

quotedExpr :: Parser QuotedExpr
quotedExpr = intrinsic <|> extrinsic
  where
    intrinsic =
      token Token.LParen
        *> token (Token.Symbol "quote")
        *> (QuotedExpr <$> some atom)
        <* token Token.RParen
    extrinsic =
      token Token.Quote
        *> token Token.LParen
        *> (QuotedExpr <$> some atom)
        <* token Token.RParen

expr :: Parser Expr
expr = (Normal <$> normalExpr) <|> (Quoted <$> quotedExpr)

ast :: Parser AST
ast = AST <$> many expr

data ParseError = FailedParse | Ambiguous [AST] deriving (Show)

parser :: [Token] -> Either ParseError AST
parser input = case map fst $ filter (null . snd) $ runParser ast input of
  [] -> Left FailedParse
  [res] -> Right res
  possibilities -> Left $ Ambiguous possibilities

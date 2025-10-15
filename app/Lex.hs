-- https://cronokirby.com/Posts/Haskell-in-Haskell-2-Lexing

module Lex (lexer, Token (..)) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (maybeToList)

data Token
  = LParen
  | RParen
  | Quote
  | StringLiteral String
  | IntegerLiteral Integer
  | FloatLiteral Float
  | Symbol String
  deriving (Show, Eq)

data LexError
  = Unexpected Char
  | UnexpectedEOF
  deriving (Show)

newtype Lexer a = Lexer
  { runLexer :: String -> Maybe (a, String)
  }

instance Functor Lexer where
  fmap f (Lexer l) = Lexer $ fmap (first f) . l

instance Applicative Lexer where
  pure x = Lexer $ \input -> Just (x, input)
  Lexer lF <*> Lexer lA = Lexer $ \input -> do
    (f, rest) <- lF input
    (a, rest') <- lA rest
    return (f a, rest')

instance Alternative Lexer where
  empty = Lexer $ const Nothing
  Lexer lA <|> Lexer lB = Lexer $ \input ->
    case (lA input, lB input) of
      (res, Nothing) -> res
      (Nothing, res) -> res
      (a@(Just (_, restA)), b@(Just (_, restB))) ->
        if length restA <= length restB then a else b

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer $ \case
  c : cs | p c -> Just (c, cs)
  _ -> Nothing

char :: Char -> Lexer Char
char c = satisfies (== c)

whitespace :: Lexer ()
whitespace = void $ many (satisfies isSpace)

syntax :: Lexer Token
syntax =
  (LParen <$ char '(')
    <|> (RParen <$ char ')')
    <|> (Quote <$ char '\'')

stringLiteral :: Lexer Token
stringLiteral = StringLiteral <$> (char '"' *> many (satisfies (/= '"')) <* char '"')

numberLiteral :: Lexer Token
numberLiteral = tokenify <$> number
  where
    tokenify :: String -> Token
    tokenify raw =
      if '.' `elem` raw
        then
          FloatLiteral $ read raw
        else IntegerLiteral $ read raw
    minus = char '-'
    prefix = maybeToList <$> optional minus
    body = some (satisfies isDigit <|> char '.')
    number = (++) <$> prefix <*> body

literal :: Lexer Token
literal = stringLiteral <|> numberLiteral

symbol :: Lexer Token
symbol = Symbol <$> name
  where
    nameBegin = satisfies isAlpha
    nameRest = many $ satisfies isAlphaNum
    regularName = (:) <$> nameBegin <*> nameRest
    specialName = (: []) <$> satisfies (`elem` ['+', '-', '*', '/'])
    name = regularName <|> specialName

token :: Lexer Token
token = whitespace *> (syntax <|> symbol <|> literal) <* whitespace

lexer :: String -> Either LexError [Token]
lexer input = case runLexer (many token) input of
  Just (tokens, []) -> Right tokens
  Just (_, ch : _) -> Left $ Unexpected ch
  Nothing -> Left UnexpectedEOF

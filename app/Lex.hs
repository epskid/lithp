-- https://cronokirby.com/Posts/Haskell-in-Haskell-2-Lexing

module Lex (lexer, Token (..)) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (maybeToList)

(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

(<++>) :: (Applicative f) => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

data Token
  = LParen
  | RParen
  | Quote
  | String String
  | Integer Integer
  | Float Float
  | Symbol String
  deriving (Show)

data LexError
  = Unexpected Char
  | UnexpectedEOF
  deriving (Show)

unexpected :: String -> LexError
unexpected [] = UnexpectedEOF
unexpected (c : _) = Unexpected c

newtype Lexer a = Lexer
  { runLexer :: String -> Either LexError (a, String)
  }

instance Functor Lexer where
  fmap f (Lexer l) = Lexer (fmap (first f) . l)

instance Applicative Lexer where
  pure x = Lexer (\input -> Right (x, input))
  Lexer lF <*> Lexer lA = Lexer $ \input -> do
    (f, rest) <- lF input
    (a, rest') <- lA rest
    return (f a, rest')

instance Alternative Lexer where
  empty = Lexer (Left . unexpected)
  Lexer lA <|> Lexer lB = Lexer $ \input ->
    case (lA input, lB input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (a@(Right (_, restA)), b@(Right (_, restB))) ->
        if length restA <= length restB then a else b

satisfies :: (Char -> Bool) -> Lexer Char
satisfies p = Lexer $ \case
  c : cs | p c -> Right (c, cs)
  rest -> Left $ unexpected rest

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
stringLiteral = String <$> (char '"' *> many (satisfies (/= '"')) <* char '"')

numberLiteral :: Lexer Token
numberLiteral = tokenify <$> (maybeToList <$> optional minus) <++> some (satisfies isDigit <|> char '.')
  where
    tokenify :: String -> Token
    tokenify raw =
      if '.' `elem` raw
        then
          Float $ read raw
        else Integer $ read raw
    minus = char '-'

literal :: Lexer Token
literal = stringLiteral <|> numberLiteral

symbol :: Lexer Token
symbol = Symbol <$> (regularName <|> specialName)
  where
    nameBegin = satisfies isAlpha
    nameRest = many $ satisfies isAlphaNum
    regularName = nameBegin <:> nameRest
    specialName = (: []) <$> satisfies (`elem` ['+', '-', '*', '/'])

token :: Lexer Token
token = whitespace *> (syntax <|> literal <|> symbol) <* whitespace

lexer :: String -> Either LexError ([Token], String)
lexer = runLexer (some token)

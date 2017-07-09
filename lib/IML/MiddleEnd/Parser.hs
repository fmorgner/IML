module IML.MiddleEnd.Parser where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe
import IML.FrontEnd.Tokens

newtype Parser a = Parser { runParser :: [Token] -> Maybe (a, [Token]) }

instance Functor Parser where
  fmap f = (>>= return . f)

instance Applicative Parser where
  pure x      = Parser (\ts -> Just (x, ts))
  lhs <*> rhs = lhs >>= \f -> f `fmap` rhs

instance Monad Parser where
  fail _  = Parser $ const Nothing
  p >>= f = Parser (\ts -> case runParser p ts of
                             Nothing -> Nothing
                             Just(x, toks) -> runParser (f x) toks)

instance Monoid a => Monoid (Parser a) where
  mempty            = return mempty
  lhs `mappend` rhs = do
    x <- lhs
    y <- rhs
    return $ x `mappend` y

instance Alternative Parser where
  empty = Parser $ const Nothing
  some p = many p <|> return []
  many p = do
    v <- p
    vs <- some p
    return (v : vs)
  lhs <|> rhs = Parser (\ts -> case runParser lhs ts of
                                 Nothing -> runParser rhs ts
                                 x -> x)

infixl 1 <<
(<<) :: Parser a -> Parser b -> Parser a
lhs << rhs = do
  res <- lhs
  rhs
  return res

module IML.MiddleEnd.Parser
  (Parser
  ) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe
import IML.FrontEnd.Tokens

newtype Parser a = Parser { runParser :: [Token] -> Maybe (a, [Token]) }

instance Functor Parser where
  fmap f = (>>= return . f)

instance Applicative Parser where
  lhs <*> rhs = Parser (\ts -> case runParser lhs ts of
                          Nothing -> Nothing
                          Just(f, toks) -> runParser (f `fmap` rhs) toks)
  pure x      = Parser (\ts -> Just (x, ts))

instance Monad Parser where
  fail _  = Parser $ const Nothing
  p >>= f = Parser (\ts -> case runParser p ts of
                             Nothing -> Nothing
                             Just(x, toks) -> runParser (f x) toks)

instance Monoid a => Monoid (Parser a) where
  mempty            = return mempty
  lhs `mappend` rhs = Parser (\ts -> case runParser lhs ts of
                                       Nothing -> Nothing
                                       Just(x, toks) -> case runParser rhs toks of
                                                          Nothing -> Nothing
                                                          Just(y, toks) -> Just(x `mappend` y, toks))

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

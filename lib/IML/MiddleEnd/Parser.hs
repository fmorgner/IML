{-
 - Copyright (c) 2017, Felix Morgner
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 -     * Redistributions of source code must retain the above copyright
 -       notice, this list of conditions and the following disclaimer.
 -
 -     * Redistributions in binary form must reproduce the above
 -       copyright notice, this list of conditions and the following
 -       disclaimer in the documentation and/or other materials provided
 -       with the distribution.
 -
 -     * Neither the name of Felix Morgner nor the names of other
 -       contributors may be used to endorse or promote products derived
 -       from this software without specific prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 - "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 - LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 - A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 - OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 - SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 - LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 - DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 - THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 - (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 - OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}

{-# LANGUAGE LambdaCase #-}

{-|
Module      : IML.MiddleEnd.Parser
Description : The IML parser type
Copyright   : (c) Felix Morgner, 2017
License     : 3-clause BSD
Maintainer  : felis.morgner@gmail.com

This module contains the IML parser type and its instances.
-}

module IML.MiddleEnd.Parser where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe
import IML.FrontEnd.Tokens

newtype Parser a =
  Parser {
{-|
Run the given 'Parser' on a list of tokens. If the parse succeedes, this
function returns a tuple consisting of the parse result and the a list of
remaining tokens. If the parse fails, 'Nothing' is returned.
-}
            runParser :: [Token] -> Maybe (a, [Token])
         }

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

{-|
Combine two parsers into a new parser, that executes the left-hand one followed
by the right-hand one. The result of the right-hand parser is ignored, while the
result of the left-hand one is returned.
-}
infixl 1 <<
(<<) :: Parser a -> Parser b -> Parser a
lhs << rhs = do
  res <- lhs
  rhs
  return res

{-|
Attach a predicate to a parser. If the parsed token does __not__ fulfill the
predicate, the parser fails. Otherwise the parsed token is returned.
-}
infix 1 ?=?
(?=?) :: (Token -> Bool) -> Parser Token
(?=?) pred = do
  res <- Parser $ \case
                   [] -> Nothing
                   (t:ts) -> Just (t, ts)
  if pred res
     then return res
     else Parser $ const Nothing

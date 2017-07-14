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

module IML.BackEnd.Environment where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Functor

newtype Environment a b = E { get :: a -> Maybe b }

instance Functor (Environment a) where
  fmap fn state = E $ fmap fn . get state

instance Applicative (Environment a) where
  pure val = E $ const $ Just val
  lhs <*> rhs = E (\x -> get lhs x <*> get rhs x)

instance Monoid (Environment a b) where
  mempty = E $ const Nothing
  E m1 `mappend` E m2 = E (\x -> m1 x <|> m2 x)

instance Monad (Environment a) where
  env >>= fun = E $ \x -> get (dofun $ get env x) x
    where dofun (Just x) = fun x
          dofun Nothing  = mempty

set :: Eq a => Environment a b -> (a, b) -> Environment a b
set old (key, val) = E newMapping
  where newMapping key' | key' == key = Just val
                        | otherwise   = get old key'

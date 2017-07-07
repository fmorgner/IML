module Environment where

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

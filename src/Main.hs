import Environment
import Data.Char

listEnv env = map (get env)

fstIds = ['a', 'b', 'c']
sndIds = ['d', 'e', 'f']

fstVals = [2,  42, 21]
sndVals = [8, -23, 18]

fns   = [(*2), (+ 4), subtract 3]

fstEnv = foldl set mempty $ zip fstIds fstVals
sndEnv = foldl set mempty $ zip sndIds sndVals
thdEnv = foldl set mempty $ zip fstIds sndVals
fnsEnv = foldl set mempty $ zip fstIds fns

-- Functor demo
doubledEnv = fmap (*2) fstEnv

-- Applicative demo
funcedEnv   = fnsEnv <*> fstEnv
funcedEnv'  = fnsEnv <*> sndEnv
funcedEnv'' = pure (*2) <*> fstEnv

-- Monoid demo
appendedEnv = fstEnv `mappend` sndEnv
appendedEnv' = mconcat [fstEnv, sndEnv, thdEnv]

-- Monad demo
wrappedEnv = sndEnv >>= pure
wrappedEnv' = sndEnv >>= idToInt
  where idToInt _ = E $ \x -> Just $ ord x

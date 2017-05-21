module MyList where

import Prelude hiding ((>>=), (<*>), return, fmap, pure)

class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  fmap = map

class (MyFunctor f) => MyApplicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance MyApplicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

class MyApplicative m => MyMonad m where
  return ::   a                 -> m a
  (>>=)  :: m a -> (  a -> m b) -> m b

instance MyMonad [] where
  return x  = [x]
  xs >>= f = [y | x <- xs, y <- f x]

testFunctor = fmap (+3) [5]
testApplicative = [(+3)] <*> [5]
testMonad = [1, 2, 3] >>= g >>= f
  where
    f x = [x-1, x, x+1]
    g x = [-x, x]

main = do
  (putStrLn . show) testFunctor
  (putStrLn . show) testApplicative
  (putStrLn . show) testMonad
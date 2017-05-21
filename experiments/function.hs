module MyFunction where

import Prelude hiding ((>>=), (<*>), return, fmap, pure)

class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b

-- (a -> b) -> (r -> a) -> (r -> b)
instance MyFunctor ((->) r) where
  fmap f g = f . g

class (MyFunctor f) => MyApplicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- (r -> a -> b) -> (r -> a) -> (r -> b)
instance MyApplicative ((->) r) where
  pure = const
  f <*> g = \r -> f r (g r)

class MyApplicative m => MyMonad m where
  return ::   a                 -> m a
  (>>=)  :: m a -> (  a -> m b) -> m b

-- (r -> a) -> (a -> r -> b) -> (r -> b)
instance MyMonad ((->) r) where
  return = const
  f >>= g = \r -> g (f r) r

testFunctor = fmap (+1) (+3) 5
testApplicative = (f <*> g) e
  where
    f = (\e a -> a + e)
    g = (\e -> 2)
    e = 5

-- (r -> a) -> (a -> r -> b) -> (r -> b)
testMonad = (f >>= g) e
  where
    -- (r -> a)
    f = \e -> e
    -- (a -> r -> b)
    g = \x e -> x + e
    e = 5

main = do
  (putStrLn . show) testFunctor
  (putStrLn . show) testApplicative
  (putStrLn . show) testMonad

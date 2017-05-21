module MyEither where

import Prelude hiding ((>>=), (<*>), return, fmap, pure)

data MyEither a b = MyLeft a | MyRight b deriving Show

class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b

instance MyFunctor (MyEither a) where
  fmap f (MyRight x) = MyRight (f x)
  fmap _ (MyLeft x)  = MyLeft x

class (MyFunctor f) => MyApplicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance MyApplicative (MyEither a) where
  pure              = MyRight
  (MyLeft e) <*> _  = MyLeft e
  (MyRight f) <*> r = fmap f r

class MyApplicative m => MyMonad m where
  return ::   a                 -> m a
  (>>=)  :: m a -> (  a -> m b) -> m b

instance MyMonad (MyEither a) where
  return   = MyRight
  (MyLeft e) >>= _ = MyLeft e
  (MyRight x) >>= f = f x

testFunctor :: (MyEither String Integer)
testFunctor = fmap (+3) (MyRight 5)

testApplicative :: (MyEither String Integer)
testApplicative = MyRight (+3) <*> (MyRight 5)

testMonad :: (MyEither String Integer)
testMonad = (MyRight 3) >>= add5
  where add5 x = MyRight (x + 5)

main = do
  (putStrLn . show) testFunctor
  (putStrLn . show) testApplicative
  (putStrLn . show) testMonad

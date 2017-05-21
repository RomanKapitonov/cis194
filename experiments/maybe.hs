module MyMaybe where

import Prelude hiding ((>>=), (<*>), return, fmap, pure)

data MyMaybe a = MyJust a | MyNothing deriving Show

class MyFunctor f where
  fmap :: (a -> b) -> f a -> f b

instance MyFunctor MyMaybe where
  fmap f MyNothing  = MyNothing
  fmap f (MyJust x) = MyJust (f x)

class (MyFunctor f) => MyApplicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance MyApplicative MyMaybe where
  pure x = MyJust x
  (MyJust f) <*> (MyJust x) = MyJust (f x)

class MyApplicative m => MyMonad m where
  return ::   a                 -> m a
  (>>=)  :: m a -> (  a -> m b) -> m b

instance MyMonad MyMaybe where
  return = MyJust
  (MyJust x) >>= f = f x
  (MyNothing) >>= _ = MyNothing

testFunctor = fmap (+3) (MyJust 5)
testApplicative = (MyJust (+3)) <*> (MyJust 5)
testMonad = (MyJust 3) >>= add5
  where add5 x = MyJust (x + 5)

main = do
  (putStrLn . show) testFunctor
  (putStrLn . show) testApplicative
  (putStrLn . show) testMonad

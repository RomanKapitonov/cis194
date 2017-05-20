data MyMaybe a = MyJust a | MyNothing deriving Show

class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

instance MyFunctor MyMaybe where
  fmap' f MyNothing  = MyNothing
  fmap' f (MyJust x) = MyJust (f x)

class (MyFunctor f) => MyApplicative f where
  pure' :: a -> f a
  ap'   :: f (a -> b) -> f a -> f b

instance MyApplicative MyMaybe where
  pure' x = MyJust x
  (MyJust f) `ap'` (MyJust x) = MyJust (f x)

testFunctor = fmap' (+3) (MyJust 5)
testApplicative = (MyJust (+3)) `ap'` (MyJust 5)

main = do
  (putStrLn . show) testFunctor
  (putStrLn . show) testApplicative

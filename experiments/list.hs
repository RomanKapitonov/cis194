class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  fmap' = map

class (MyFunctor f) => MyApplicative f where
  pure' :: a -> f a
  ap'   :: f (a -> b) -> f a -> f b

instance MyApplicative [] where
  pure' x = [x]
  fs `ap'` xs = [f x | f <- fs, x <- xs]

testFunctor = fmap' (+3) [5]
testApplicative = [(+3)] `ap'` [5]

main = do
  (putStrLn . show) testFunctor
  (putStrLn . show) testApplicative

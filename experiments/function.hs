class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

-- (a -> b) -> (r -> a) -> (r -> b)
instance MyFunctor ((->) r) where
  fmap' f g = f . g

class (MyFunctor f) => MyApplicative f where
  pure' :: a -> f a
  ap'   :: f (a -> b) -> f a -> f b

-- (r -> a -> b) -> (r -> a) -> (r -> b)
instance MyApplicative ((->) r) where
  pure' = const
  ap' f g e = f e (g e)

testFunctor = fmap' (+1) (+3) 5
testApplicative = ap' f g e
  where
    f = (\e a -> a + e)
    g = (\e -> 2)
    e = 5

main = do
  putStrLn $ show testFunctor
  putStrLn $ show testApplicative

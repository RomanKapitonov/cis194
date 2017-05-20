data MyEither a b = MyLeft a | MyRight b deriving Show

class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

instance MyFunctor (MyEither a) where
  fmap' f (MyRight x) = MyRight (f x)
  fmap' _ (MyLeft x)  = MyLeft x

class (MyFunctor f) => MyApplicative f where
  pure' :: a -> f a
  ap'   :: f (a -> b) -> f a -> f b

instance MyApplicative (MyEither a) where
  pure'               = MyRight
  (MyLeft e) `ap'` _  = MyLeft e
  (MyRight f) `ap'` r = fmap' f r

testFunctor :: (MyEither String Integer)
testFunctor = fmap' (+3) (MyRight 5)

testApplicative :: (MyEither String Integer)
testApplicative = MyRight (+3) `ap'` (MyRight 5)

main = do
  putStrLn $ show testFunctor
  putStrLn $ show testApplicative

class MMonoid a where
  mmempty :: a
  mmappend :: a -> a -> a
  mmconcat :: [a] -> a

newtype All = All { getAll :: Bool } deriving Show
newtype Any = Any { getAny :: Bool } deriving Show

instance MMonoid All where
  mmempty = All True
  mmappend (All x) (All y) = All (x && y)
  mmconcat = foldr mmappend mmempty

instance MMonoid Any where
  mmempty = Any False
  mmappend (Any x) (Any y) = Any (x || y)
  mmconcat = foldr mmappend mmempty

listToAll :: [Bool] -> [All]
listToAll xs = map (All) xs

listToAny :: [Bool] -> [Any]
listToAny xs = map (Any) xs

testAll :: [Bool] -> All
testAll = mmconcat . listToAll

testAny :: [Bool] -> Any
testAny = mmconcat . listToAny

instance MMonoid b => MMonoid (a -> b) where
  mmempty _ = mmempty
  mmappend f g x = f x `mmappend` g x
  mmconcat = foldr mmappend mmempty

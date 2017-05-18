import Data.Functor
-- Implement Functor instances for Either e and ((->) e).

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

-- fmap :: (a -> b) -> f a -> f b
-- (a -> b) -> (e -> a) -> (e -> b)
-- fmap f g = \x -> f (g x)
instance Functor ((->) e) where
  fmap f g = f . g

-- Implement Functor instances for ((,) e) and for Pair, defined as
-- Explain their similarities and differences.

instance Functor ((,) e) where
  fmap f (e, x) = (e, f x)

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- Implement a Functor instance for the type ITree, defined as
data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf g) = (Leaf (f . g))
  fmap f (Node xs) = Node (map (fmap f) xs)

-- Give an example of a type of kind * -> * which cannot be made an instance of Functor (without using undefined).
-- Is this statement true or false?
-- fmap :: (a -> b) -> (a -> Int) -> (b -> Int) - THIS IS NOT POSSIBLE
-- No rule to cast map Int to a
data Something a = Something ((->) a String)

-- The composition of two Functors is also a Functor.
-- If false, give a counterexample; if true, prove it by exhibiting some appropriate Haskell code.

-- :t fmap . fmap
-- (Functor f, Functor f1) => (a -> b) -> f1 (f a) -> f1 (f b)
newtype ListOfMaybe a = ListOfMaybe [Maybe a]
newtype MaybeOfList a = MaybeOfList (Maybe [a])

instance Functor ListOfMaybe where
  fmap f (ListOfMaybe xs) = ListOfMaybe (fmap (fmap f) xs)

instance Functor MaybeOfList where
  fmap f (MaybeOfList xs) = MaybeOfList (fmap (fmap f) xs)

data IntAndSmth a = IntAndSmth Int a deriving (Show, Eq)

instance Functor IntAndSmth where
  fmap f (IntAndSmth _ x) = IntAndSmth 1 (f x)

-- Although it is not possible for a Functor instance to satisfy the first Functor law
-- but not the second (excluding undefined), the reverse is possible.
-- Give an example of a (bogus) Functor instance which satisfies the second law but not the first.

-- fmap id = id
testIdentityEquals = (==) (IntAndSmth 5 5) (fmap id (IntAndSmth 5 5))
-- fmap (g . h) = (fmap g) . (fmap h)
testCompositionHolds = combined == splitted
  where
    combined = fmap ((+2) . (*2)) (IntAndSmth 4 4)
    splitted = fmap (+2) $ fmap (*2) (IntAndSmth 4 4)

-- Which laws are violated by the evil Functor instance for list shown above: both laws,
-- or the first law alone? Give specific counterexamples.
-- BOTH LAWS ARE VIOLATED

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- fmap id = id
-- fmap (g . h) = (fmap g) . (fmap h)
-- Evil Functor instance - The following functor violates fmap id = id law
instance Functor' [] where
  fmap' _ [] = []
  fmap' g (x:xs) = g x : g x : fmap g xs

-- fmap id = id
testEvilIdentity = [1,2,3] == fmap' id [1,2,3]
testEvilComposition = combined == splitted
  where
    combined = fmap' ((+2) . (*2)) [1,2,3]
    splitted = (fmap' (+2) . fmap' (*2)) [1,2,3]

testAliasedFmap = (+3) <$> [1..3] -- [4,5,6]
testContextWrap = (Just 1) $> 5   -- Just 5
testVoid        = void Just 5     -- ()

-- (Tricky) One might imagine a variant of the interchange law that says something about
-- applying a pure function to an effectful argument. Using the above laws, prove that
-- pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- how ??????????

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   infixl 4 <*>, *>, <*
--   (<*>) :: f (a -> b) -> f a -> f b

--   (*>) :: f a -> f b -> f b
--   a1 *> a2 = (id <$ a1) <*> a2

--   (<*) :: f a -> f b -> f a
--   (<*) = liftA2 const

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show, Eq)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

-- fmap id = id
testFunctorZipListIdentity = (==) (id (ZipList [1])) (ZipList [1])
-- fmap (g . h) = (fmap g) . (fmap h)
testFunctorZipListComposition = combined == splitted
  where
    combined = fmap ((+2) . (*2)) (ZipList [1,2,3])
    splitted = (fmap (+2) . fmap (*2)) (ZipList [1,2,3])

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

-- The identity law:
-- pure id <*> v = v
testApplicativeZipListPurity = identity == self
  where
    identity = pure id <*> (ZipList [1..10])
    self     = ZipList [1..10]

-- Homomorphism:
-- pure f <*> pure x = pure (f x)
-- Intuitively, applying a non-effectful function to a non-effectful argument in an effectful
-- context is the same as just applying the function to the argument and then injecting the
-- result into the context with pure.

testApplicativeZipListHomomorphism = splitPure == combinedPure
  where
    splitPure    = (pure (+2)) <*> (pure 2 :: ZipList Int)
    combinedPure = (pure ((+2) 2) :: ZipList Int)

-- Interchange:
-- u <*> pure y = pure ($ y) <*> u
-- Intuitively, this says that when evaluating the application of an effectful function to a
-- pure argument, the order in which we evaluate the function and its argument doesn't matter.

testApplicativeZipListInterchange = forward == backward
  where
    forward  = (ZipList [(+1), (+2)]) <*> pure 1
    backward = pure ($ 1) <*> (ZipList [(+1), (+2)])

-- Composition:
-- u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
-- This one is the trickiest law to gain intuition for. In some sense it is expressing a sort
-- of associativity property of (<*>). The reader may wish to simply convince themselves that
-- this law is type-correct.

testApplicativeZipListComposition = forward == backward
  where
    forward  = (ZipList [(+1)]) <*> ((ZipList [(*2)]) <*> (ZipList [1]))
    backward = pure (.) <*> (ZipList [(+1)]) <*> (ZipList [(*2)]) <*> (ZipList [1])

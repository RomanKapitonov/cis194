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

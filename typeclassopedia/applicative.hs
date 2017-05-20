import Data.Functor

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

-- Implement a function
-- sequenceAL :: Applicative f => [f a] -> f [a]
-- . There is a generalized version of this, sequenceA, which works for any Traversable
-- (see the later section on Traversable), but implementing this version specialized to lists
-- is a good exercise.

-- sequenceAL [(+3),(+2),(+1)] == \a -> [(a + 3), (a + 2), (a + 1)]
-- [(Just (+1)), (Just (+5))] -> Just [(+3), (+5)]
-- sequenceA [(+3),(+2),(+1)] 3
-- fmap :: (a -> b)

-- (<*>) :: [a -> b] -> [a] -> [b]
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL []     = pure []
sequenceAL (x:xs) = ((fmap (:) x) <*>) (sequenceAL xs)

-- liftA2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)

-- CONTEXT == ((->) r)
-- sequenceA [(+3),(+2),(+1)] = (:) <$> (+3) <*> sequenceA [(+2),(+1)]
-- sequenceA [(+2),(+1)]      = (:) <$> (+2) <*> sequenceA [(+1)]
-- sequenceA [(+1)]           = (:) <$> (+1) <*> sequenceA []
-- sequenceA []               = pure []

-- Let's look at the last line. pure [] takes an empty list and puts it inside some applicative 
-- structure. As we've just seen, the applicative structure in this case is ((->) r). 
-- Because of this, sequenceA [] = pure [] = const [].

-- Now, line 3 can be written as:

-- sequenceA [(+1)] = (:) <$> (+1) <*> const []

-- sequenceAL :: Applicative f => [f a] -> f [a]
-- f = (->) r
-- [r -> a] -> (r -> [a])

-- Functor f => (a -> b) -> f a -> f b
-- Applicative f => f (a -> b) -> f a -> f b

-- sequenceA [(+3),(+2),(+1)] == \a -> [(a + 3), (a + 2), (a + 1)]
--                        fmap (:) (Just 1) <*> pure []
-- fmap (:) (Just 2) <*> (fmap (:) (Just 1) <*> pure [])

-- [Just 1, Just 2, Just 3] -> Just [1, 2, 3]
-- (Just 1) <*> pure [Just 3]

-- :t (pure [])
-- (pure [])                  :: Applicative f => f [a -> b]
-- :t (<*> pure [])
-- (<*> pure [])              :: Applicative f => f ([a -> b] -> b) -> f b
-- :t ((3+) <*> pure [])
-- ((3+) <*> pure [])         :: Num ([a -> b] -> b) => ([a -> b] -> b) -> b
-- :t ((<$>) (3+) <*> pure [])
-- ((<$>) (3+) <*> pure [])   :: Num b => ([a -> b] -> b) -> b
-- :t ((:) <$> (3+) <*> pure [])
-- ((:) <$> (3+) <*> pure []) :: Num a => a -> [a]



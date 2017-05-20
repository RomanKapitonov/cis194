class Applicative m => MyMonad m where
  return :: a -> m a
  (>>==)  :: m a -> (a -> m b) -> m b
  (>>>)   :: m a -> m b -> m b
  m >>> n = m >>== \_ -> n

--   fail   :: String -> m a

data W a = W a deriving Show

-- RETURN WRAPPERD VALUE
return' :: a -> W a
return' x = W x

-- GET FUNCTION AND RETURN FUNCTION ON WRAPPED VALUES
fmap' :: (a -> b) -> (W a -> W b)
fmap' f (W x) = W (f x)

a = W 1
b = fmap' (+1) a

f :: Int -> W Int
f x = W (x + 1)

bind' :: (a -> W b) -> W a -> W b
bind' f (W x) = f x

c = bind' f (f 1)
d = bind' f (bind' f (f 1))

-- fmap' f = bind (return' . f)

-- So here are a couple of exercises:

-- (1) define a function g :: Int -> W Int -> W Int so that
-- g x (W y) = W (x+y). Obviously that definition won't do -
-- the left hand side has a W y pattern so it's actually unwrapping.
-- Rewrite this function so that the only unwrapping that happens is carried out by bind.
g :: Int -> W Int -> W Int
g x y = bind' (return' . (+x)) y

-- (2) define a function h :: W Int -> W Int -> W Int so that h (W x) (W y) = W (x+y). Again, no unwrapping.
h :: W Int -> W Int -> W Int
h x y = bind' (\x -> g x y) x

-- Implement a Monad instance for the list constructor, []. Follow the types!

instance MyMonad [] where
  return x = [x]
  xs >>== f = [y | x <- xs, y <- f x]

-- Implement a Monad instance for ((->) e).
instance MyMonad ((->) e) where
  -- return :: a -> m a
  -- return :: a -> (e -> a)
  return = const
  -- (>>==)  :: m a -> (a -> m b) -> m b
  -- (>>==)  :: (e -> a) -> (a -> e -> b) -> (e -> b)
  f >>== k = \e -> k (f e) e

-- Implement Functor and Monad instances for Free f, defined as
data Free f a = Var a
              | Node (f (Free f a))
-- You may assume that f has a Functor instance. This is known as the free monad built from the functor f.

instance Functor f => Functor (Free f) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Var x) = Var (f x)
  fmap f (Node x) = Node $ fmap (\e -> fmap f e) x

-- class (Functor f) => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative (Free f) where
  pure = Var



instance MyMonad (Free f) where
  return = undefined

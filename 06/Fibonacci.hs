{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type
-- fib :: Integer -> Integer
-- so that fib n computes the nth Fibonacci number Fn.
-- Now use fib to define the infinite list of all Fibonacci numbers,
-- fibs1 :: [Integer]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
-- When I said “we” in the previous sentence I actually meant “you”.
-- Your task for this exercise is to come up with more efficient implementation.
-- Specifically, define the infinite list
-- fibs2 :: [Integer]
-- so that it has the same elements as fibs1, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from the Prelude as appropriate.

fibs2 :: [Int]
fibs2 = [x | x <- zipWith (+) (1:fibs2) (0:1:fibs2)]

fibs2' :: [Int]
fibs2' = do
  x <- zipWith (+) (1:fibs2') (0:1:fibs2')
  return x

fibs2'' :: [Int]
fibs2'' = 1 : 1 : [a + b | (a, b) <- zip fibs2'' (tail fibs2'')]

-- Exercise 3
-- • Define a data type of polymorphic streams, Stream.

data Stream a = Cons a (Stream a)

-- • Write a function to convert a Stream to an infinite list,
-- streamToList :: Stream a -> [a]

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- • To test your Stream functions in the succeeding exercises, it will be
-- useful to have an instance of Show for Streams. However, if you put
-- deriving Show after your definition of Stream, as one usually does,
-- the resulting instance will try to print an entire Stream—which,
-- of course, will never finish. Instead, you should make your own
-- instance of Show for Stream,
-- instance Show a => Show (Stream a) where
-- show ...
-- which works by showing only some prefix of a stream (say, the
-- first 20 elements).

instance Show a => Show (Stream a) where
  show = show . streamToList

-- Exercise 4
-- Let’s create some simple tools for working with Streams.
-- • Write a function
-- streamRepeat :: a -> Stream a

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- which generates a stream containing infinitely many copies of the
-- given element.
-- • Write a function
-- streamMap :: (a -> b) -> Stream a -> Stream b

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- which applies a function to every element of a Stream.
-- • Write a function
-- streamFromSeed :: (a -> a) -> a -> Stream a

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- which generates a Stream from a “seed” of type a, which is the
-- first element of the stream, and an “unfolding rule” of type a -> a
-- which specifies how to transform the seed into a new seed, to be
-- used for generating the rest of the stream.


-- Exercise 5
-- Now that we have some tools for working with streams, let’s create
-- a few:
-- • Define the stream
-- nats :: Stream Integer

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- which contains the infinite list of natural numbers 0, 1, 2, . . .

-- • Define the stream
-- ruler :: Stream Integer

-- Hint: define a function
-- interleaveStreams which alternates
-- the elements from two streams. Can
-- you use this function to implement
-- ruler in a clever way that does not have
-- to do any divisibility testing?

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = from 0

-- 0 interleaved with (1 interleaved with (2 interleaved with ( ... )))
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5
from :: Integer -> Stream Integer
from n = interleaveStreams (streamRepeat n) (from $ n + 1)

-- which corresponds to the ruler function
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
-- where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n.

-- Fibonacci numbers via generating functions (extra credit)
-- This section is optional but very cool, so if you have time I hope you
-- will try it. We will use streams of Integers to compute the Fibonacci
-- numbers in an astounding way.
-- The essential idea is to work with generating functions of the form
-- a0 + a1x + a2x
-- 2 + · · · + anx
-- n + . . .
-- where x is just a “formal parameter” (that is, we will never actually
-- substitute any values for x; we just use it as a placeholder) and all the
-- coefficients ai are integers. We will store the coefficients a0, a1, a2, . . .
-- in a Stream Integer.

-- Exercise 6

-- First, define
-- x :: Stream Integer
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

-- AB = (a0 + xA') B = a0 B + x A'B = a0 (b0 + xB') + x(A'B) = (a0 * b0) + x(a0 * B' + A'B)
instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons x xs)= Cons (-x) (negate xs)
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + a' * b )


-- Q = (a0 / b0) + x((1 / b0) (A' − QB')).
instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    (Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
            (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = fetch $ (Matrix 1 1 1 0) ^ n
  where fetch (Matrix _ f _ _) = f

fibs4 = map fib4 [0..]
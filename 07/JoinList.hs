{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Scrabble
import Data.Monoid
import StringBuffer
import Buffer
import Editor

-- import Data.Monoid
-- import Buffer
-- import Editor
-- import Sized
-- import Scrabble

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1 We first consider how to write some simple operations
-- on these JoinLists. Perhaps the most important operation we will
-- consider is how to append two JoinLists. Previously, we said that
-- the point of JoinLists is to represent append operations as data, but
-- what about the annotations? Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived
-- from those of the two arguments.
-- (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- You may find it helpful to implement a helper function
-- tag :: Monoid m => JoinList m a -> m
-- which gets the annotation at the root of a JoinList.

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append ((tag x) `mappend` (tag y)) x y

-- Exercise 2 The first annotation to try out is one for fast indexing
-- into a JoinList. The idea is to cache the size (number of data elements)
-- of each subtree. This can then be used at each step to determine
-- if the desired index is in the left or the right branch.
-- We have provided the Sized module that defines the Size type,
-- which is simply a newtype wrapper around an Int. In order to make
-- Sizes more accessible, we have also defined the Sized type class
-- which provides a method for obtaining a Size from a value.
-- Use the Sized type class to write the following functions.
-- 1. Implement the function

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- indexJ i = (!!? i) . jlToList

az :: JoinList Size Char
az = foldr1 (+++) $ fmap (Single (Size 1)) ['a'..'z']

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a)
  | n == 0 = Just a
  | otherwise = Nothing
indexJ n (Append m l r)
  | n >= left_size = indexJ (n - left_size) r
  | otherwise = indexJ n l
  where
    left_size = getSize . size . tag $ l

-- 2. Implement the function
-- dropJ :: (Sized b, Monoid b) =>
-- Int -> JoinList b a -> JoinList b a
-- The dropJ function drops the first n elements from a JoinList.
-- This is analogous to the standard drop function on lists. Formally,
-- dropJ should behave in such a way that
-- jlToList (dropJ n jl) == drop n (jlToList jl).

-- data JoinList m a = Empty
--   | Single m a
--   | Append m (JoinList m a) (JoinList m a)
--   deriving (Eq, Show)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r)
  | n >= left_size = dropJ (n - left_size) r
  | otherwise = dropJ n l +++ r
  where
    left_size = getSize . size . tag $ l

-- 3. Finally, implement the function
-- takeJ :: (Sized b, Monoid b) =>
-- Int -> JoinList b a -> JoinList b a
-- The takeJ function returns the first n elements of a JoinList,
-- dropping all other elements. Again, this function works similarly
-- to the standard library take function; that is, it should be the case
-- that
-- jlToList (takeJ n jl) == take n (jlToList jl).
-- Ensure that your function definitions use the size function from
-- the Sized type class to make smart decisions about how to descend
-- into the JoinList tree.

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ l r)
  | n >= left_size = l +++ takeJ (n - left_size) r
  | otherwise = takeJ n l
  where
    left_size = getSize . size . tag $ l


scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

-- class Buffer b where

--   -- | Convert a buffer to a String.
--   toString :: b -> String

--   -- | Create a buffer from a String.
--   fromString :: String -> b

--   -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
--   -- for out-of-bounds indices.
--   line :: Int -> b -> Maybe String

--   -- | @replaceLine n ln buf@ returns a modified version of @buf@,
--   --   with the @n@th line replaced by @ln@.  If the index is
--   --   out-of-bounds, the buffer should be returned unmodified.
--   replaceLine :: Int -> String -> b -> b

--   -- | Compute the number of lines in the buffer.
--   numLines :: b -> Int

--   -- | Compute the value of the buffer, i.e. the amount someone would
--   --   be paid for publishing the contents of the buffer.
--   value :: b -> Int

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList
  fromString =  foldr1 (+++) . map (\l -> Single (scoreString l, Size 1) l) . lines
  line = indexJ

  replaceLine i s l = takeJ (i-1) l +++ fromString s +++ dropJ i l

  numLines Empty = 0
  numLines (Single (_,Size n) _) = n
  numLines (Append (_,Size n) _ _) = n

  value Empty = 0
  value (Single (Score n,_) _) = n
  value (Append (Score n,_) _ _) = n

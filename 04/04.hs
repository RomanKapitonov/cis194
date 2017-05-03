import Data.List
-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style. Use wholemeal programming practices, breaking each
-- function into a pipeline of incremental transformations to an entire
-- data structure. Name your functions fun1’ and fun2’ respectively.
-- 1. fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
-- | even x = (x - 2) * fun1 xs
-- | otherwise = fun1 xs

fun1 :: [Integer] -> Integer
fun1 = product . map (const(-2)) . filter even

-- 2. fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n = n + fun2 (n ‘div‘ 2)
-- | otherwise = fun2 (3 * n + 1)
-- Hint: For this problem you may wish to use the functions iterate
-- and takeWhile. Look them up in the Prelude documentation to see
-- what they do.

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2: Folding with trees
-- Recall the definition of a binary tree data structure. The height of http://en.wikipedia.org/wiki/
-- Binary_tree a binary tree is the length of a path from the root to the deepest
-- node. For example, the height of a tree with a single node is 0; the
-- height of a tree with three nodes, whose root has two children, is 1;
-- and so on. A binary tree is balanced if the height of its left and right
-- subtrees differ by no more than 1, and its left and right subtrees are
-- also balanced.
-- You should use the following data structure to represent binary
-- trees. Note that each node stores an extra Integer representing the
-- height at that node.
-- data Tree a = Leaf
-- | Node Integer (Tree a) a (Tree a)
-- deriving (Show, Eq)
-- For this exercise, write a function
-- foldTree :: [a] -> Tree a
-- foldTree = ...
-- which generates a balanced binary tree from a list of values using
-- foldr.

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)
-- TODO: Switch to AVL sometime
insertInTree x Leaf = Node 1 (Leaf) x (Leaf)
insertInTree x (Node h left e right)
    | hl < hr   = Node h nl   e right
    | hl > hr   = Node h left e nr
    | otherwise = Node (hn + 1) nl e right
  where height Leaf = 0
        height (Node h _ _ _) = h
        hl = height left
        hr = height right
        nl = insertInTree x left
        nr = insertInTree x right
        hn = height nl

foldTree :: [a] -> Tree a
foldTree = foldr (\x acc -> insertInTree x acc) Leaf


-- Exercise 3: More folds!
-- 1. Implement a function
-- xor :: [Bool] -> Bool
-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,
-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
-- Your solution must be implemented using a fold.

xor :: [Bool] -> Bool
xor = foldr (\x y -> (x || y) && (not (x && y))) False

-- 2. Implement map as a fold. That is, complete the definition
-- map’ :: (a -> b) -> [a] -> [b]
-- map’ f = foldr ...
-- in such a way that map’ behaves identically to the standard map
-- function.

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 3. (Optional) Implement foldl using foldr. That is, complete the
-- definition
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...
-- in such a way that myFoldl behaves identically to the standard
-- foldl function.
-- Hint: Study how the application of foldr and foldl work out:
-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = undefined

-- Exercise 4: Finding primes
-- Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
-- of_Sundaram ing function composition. Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2.
-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram = ...
-- To give you some help, below is a function to compute the Cartesian
-- product of two lists. This is similar to zip, but it produces all
-- possible pairs instead of matching up the list elements. For example,
-- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
-- It’s written using a list comprehension, which we haven’t talked about
-- in class (but feel free to research them).
-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x,y) | x <- xs, y <- ys]

exclude :: Integer -> [Integer]
exclude n = [i + j + 2 * i * j | i <- [1..n], j <- [i..n]]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let excluded = exclude n in
  2:[2 * p + 1 | p <- [1..n], p `notElem` excluded]

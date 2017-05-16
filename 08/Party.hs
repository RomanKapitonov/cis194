{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Data.List
import Employee
import Data.Tree

-- Exercise 1
-- Now define the following tools for working with GuestLists:
-- 1. A function
-- glCons :: Employee -> GuestList -> GuestList
-- which adds an Employee to the GuestList (updating the cached
-- Fun score appropriately). Of course, in general this is impossible:
-- the updated fun score should depend on whether the Employee
-- being added is already in the list, or if any of their direct subordinates
-- are in the list, and so on. For our purposes, though, you
-- may assume that none of these special cases will hold: that is,
-- glCons should simply add the new Employee and add their fun
-- score without doing any kind of checks.

-- data GuestList = GL [Employee] Fun
--   deriving (Show, Eq)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (f + empFun e)

-- 2. A Monoid instance for GuestList.
-- (How is the Monoid instance supposed to work, you ask? You figure it out!)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

-- Note: that this requires creating an
-- “orphan instance” (a type class instance
-- instance C T which is defined in a
-- module which is distinct from both the
-- modules where C and T are defined),
-- which GHC will warn you about.
-- You can ignore the warning, or add
-- {-# OPTIONS_GHC -fno-warn-orphans #-}
-- to the top of your file.

-- 3. A function moreFun :: GuestList -> GuestList -> GuestList
-- which takes two GuestLists and returns whichever one of them
-- is more fun, i.e. has the higher fun score. (If the scores are equal it
-- does not matter which is returned.)

-- STRAIGHTFORWARD SOLUTION:
-- moreFun :: GuestList -> GuestList -> GuestList
-- moreFun gl1@(GL _ f1) gl2@(GL _ f2)
--   | f1 > f2   = gl1
--   | otherwise = gl2

-- Since Ord instance is define that may be done as:
-- instance Ord GuestList where
--   compare (GL _ f1) (GL _ f2) = compare f1 f2
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2
-- The Data.Tree module from the standard Haskell libraries defines
-- the type of “rose trees”, where each node stores a data element and
-- has any number of children (i.e. a list of subtrees):
-- data Tree a = Node {
-- rootLabel :: a, -- label value
-- subForest :: [Tree a] -- zero or more child trees
-- }
-- Strangely, Data.Tree does not define a fold for this type! Rectify the
-- situation by implementing
-- treeFold :: ... -> Tree a -> b
-- (See if you can figure out what type(s) should replace the dots in
-- the type of treeFold. If you are stuck, look back at the lecture notes
-- from Week 7, or infer the proper type(s) from the remainder of this
-- assignment.)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r xs) = f r (map (treeFold f) xs)

-- Exercise 3
-- Write a function
-- nextLevel :: Employee -> [(GuestList, GuestList)]
-- -> (GuestList, GuestList)
-- which takes two arguments. The first is the “boss” of the current subtree
-- (let’s call him Bob). The second argument is a list of the results
-- for each subtree under Bob. Each result is a pair of GuestLists: the
-- first GuestList in the pair is the best possible guest list with the boss
-- of that subtree; the second is the best possible guest list without the
-- boss of that subtree. nextLevel should then compute the overall best
-- guest list that includes Bob, and the overall best guest list that doesn’t
-- include Bob.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  where
    withoutBoss = mconcat $ map (uncurry moreFun) gls
    withBoss    = glCons boss $ mconcat $ map snd gls

-- Exercise 5
-- Implement main :: IO () so that it reads your company’s hierarchy
-- from the file company.txt, and then prints out a formatted guest
-- list, sorted by first name, which looks like
-- Total fun: 23924
-- Adam Debergues
-- Adeline Anselme
-- ...
-- (Note: the above is just an example of the format; it is not the correct
-- output!) You will probably find the readFile and putStrLn functions
-- useful.
-- As much as possible, try to separate out the “pure” computation
-- from the IO computation. In other words, your main function should
-- actually be fairly short, calling out to helper functions (whose types
-- do not involve IO) to do most of the work. If you find IO “infecting”
-- all your function types, you are Doing It Wrong.

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

format :: GuestList -> String
format (GL gl f) = total ++ names gl
  where
    total = "Total fun: " ++ show f ++ "\n"
    names = unlines . sort . map empName

main = do
  input <- readFile "company.txt"
  (putStrLn . format . maxFun) (read input)

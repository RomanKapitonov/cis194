-- import Golf where

import Data.List

-- Exercise 1 Hopscotch
-- Your first task is to write a function
-- skips :: [a] -> [[a]]
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.
-- For example:
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
-- Note that the output should be the same length as the input.

-- One way
-- Using function composition in filter over a list of tuples
skip :: Int -> [a] -> [a]
skip n = map snd . filter ((==0) . (`mod` n) . fst) . zip [1..]

-- Another way
-- Using folds and lambda to update the accumulator (which is a empty list at first)
skip' :: Int -> [a] -> [a]
skip' n = foldr (\(i, x) acc -> if i `mod` n == 0 then x:acc else acc) [] . zip [1..]

-- Using comprehension
skips :: [a] -> [[a]]
skips xs = [skip n xs | n <- [1..length xs]]

-- Create a list of replicas of the initial list
-- Then create a list of skips for lengths from 1 to infinity
-- then use zipWith to apply ($) a function from (map skip) to (replicate ...)
skips' :: [a] -> [[a]]
skips' xs = zipWith ($) (map skip [1..]) (replicate (length xs) xs)

-- Using fold
skips'' :: [a] -> [[a]]
skips'' xs = foldr (\(i, _) acc -> (skip i xs) : acc) [] (zip [1..] xs)

-- Using function composition as a body for fold
skips''' :: [a] -> [[a]]
skips''' xs = foldr ((:) . (`skip` xs) . fst) [] (zip [1..] xs)

-- Exercise 2 Local maxima
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, since
-- it is greater than the elements immediately before and after it (3 and
-- 1). 5 is not a local maximum since there is no element that comes
-- after it.
-- Write a function
-- localMaxima :: [Integer] -> [Integer]
-- which finds all the local maxima in the input list and returns them in
-- order. For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
  | (b > a) && (b > c) = b:localMaxima rest
  | otherwise          = localMaxima rest
  where rest = (b:c:xs)
localMaxima _ = []


-- Exercise 3 Histogram
-- For this task, write a function
-- histogram :: [Integer] -> String
-- which takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers). Your output must exactly match the output shown in the
-- examples below.

-- histogram [1,4,5,4,6,6,3,4,2,4,9]

-- wrapWithCount :: (Num t, Enum t, Eq t) => [t] -> [(t, Int)]
-- wrapWithCount xs = [(x, y) | x <- [1..9], y <- [length $ filter (x==) xs]]

-- hist :: Show t => (t, Int) -> String
-- hist (element, count) = show element ++ "=" ++ replicate count '*' ++ replicate (9 - count) ' '

histogram :: [Integer] -> String
histogram = unlines . reverse . transpose . map hist . wrapWithCount
  where hist (element, count) =
          show element ++
          "=" ++
          replicate count '*' ++
          replicate (9 - count) ' '
        wrapWithCount xs = [(x, y) | x <- [1..9], y <- [length $ filter (x==) xs]]


-- Exercise 1 We need to first find the digits of a number. Define the
-- functions

-- toDigits :: Integer -> [Integer]
-- toDigitsRev :: Integer -> [Integer]

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0          = []
  | x `elem` [1..9] = [x]
  | otherwise       = (x `mod` 10) : (toDigits (x `div` 10 ))

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

-- Exercise 2 Once we have the digits in the proper order, we need to
-- double every other one. Define a function
-- doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [if i `mod` 2 == 0 then x * 2 else x | (x, i) <- zip xs [1..]]

-- Exercise 3 The output of doubleEveryOther has a mix of one-digit
-- and two-digit numbers. Define the function
-- sumDigits :: [Integer] -> Integer

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs)
  | x < 10    =  x + sumDigits xs
  | otherwise = sumDigits $ xs ++ toDigits x

-- Exercise 4 Define the function
-- validate :: Integer -> Bool
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate number = (sumDigits . doubleEveryOther $ toDigits number) `mod` 10 == 0

-- Exercise 5 The Towers of Hanoi is a classic puzzle with a solution
-- that can be described recursively. Disks of different sizes are stacked
-- on three pegs; the goal is to get from a starting configuration with
-- all disks stacked on the first peg to an ending configuration with all
-- disks stacked on the last peg, as shown in Figure 1

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from temp to = hanoi (n - 1) from to temp ++ [(from, to)] ++ hanoi (n - 1) to temp from
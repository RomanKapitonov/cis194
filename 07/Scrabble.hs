{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid

newtype Score = Score { getScore :: Int } deriving (Eq, Ord, Num, Show)

instance Monoid Score where
  mappend = (+)
  mempty  = Score 0

score :: Char -> Score
score c
  | l `elem` "aeilnorstu" = 1
  | l `elem` "dg" = 2
  | l `elem` "bcmp" = 3
  | l `elem` "fhvwy" = 4
  | l `elem` "k" = 5
  | l `elem` "jx" = 8
  | l `elem` "qz" = 10
  | otherwise = 0
  where l = toLower c

scoreString :: String -> Score
scoreString = mconcat . map score

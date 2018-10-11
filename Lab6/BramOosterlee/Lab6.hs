module Lab6 where

import Data.List
import System.Random
import Lecture6

--Assignment 1, time 00:05
exM :: Integer -> Integer -> Integer -> Integer
exM x y n	| y == 0	= 1 `mod` n
			| otherwise = (exM x (y-1) n) * (x `mod` n)

--Assignment 2, time

--Assignment 3, time 00:05
mrComposite :: Integer -> Integer -> Bool mrComposite x n = let
(r,s) = decomp (n-1)
fs = takeWhile (/= 1)
(map (\ j -> exM x (2^j*s) n) [0..r]) in
exM x s n /= 1 && last fs /= (n-1) -- if (xs mod n) == 1 → (xs mod n)pow(2,r) == 1
-- if last fs == (n-1) → xpow(2,r)*s does not fail Fermat

composites :: [Integer]
composites = [(a, b) | a <- [1..], b <- [1..], (mrComposite a b)]

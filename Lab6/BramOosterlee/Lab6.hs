module Lab6 where

import Data.List
import System.Random
import Lecture6

--Assignment 1, time 00:05
exM :: Integer -> Integer -> Integer -> Integer
exM x y n | y == 0    = 1 `mod` n
          | otherwise = (Lab6.exM x (y-1) n) * (x `mod` n)

--Assignment 2, time
-- todo: optimize to find the composites more efficiently

--Assignment 3, time 00:20
composites :: [Integer]
composites = [n | n <- [3..], any (==True) [mrComposite x n | x <- [2..n-1]]]

--Assignment 4, time 00:40
leastCompositeFail :: Int -> IO Integer
leastCompositeFail k = lCFInternal Lab6.composites k

lCFInternal :: [Integer] -> Int -> IO Integer
lCFInternal (x:xs) k = do
						y <- primeTestsF k x
						if (not y) 
						then return x 
						else (lCFInternal xs k)

-- least composite number is 4.

-- When we increase k in primeTestsF, we increase the number of random integers we use in the
-- modular exponentiation check for k and n. By taking a larger sample size, we perform the check on more
-- numbers, and thus increase the certainty whether n is a prime.
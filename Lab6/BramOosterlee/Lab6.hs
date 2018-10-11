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

--Assignment 4, time 00:30
leastCompositeFail :: IO Integer
leastCompositeFail = lCFInternal Lab6.composites

lCFInternal :: [Integer] -> IO Integer
lCFInternal (x:xs) = do
						y <- primeTestF x
						if (not y) 
						then return x 
						else (lCFInternal xs)

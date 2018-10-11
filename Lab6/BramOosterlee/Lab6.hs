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
primeCheck :: [Integer] -> Int -> IO Integer
primeCheck (x:xs) k = do
						y <- primeTestsF k x
						if y 
						then return x 
						else (primeCheck xs k)

primeFailTest :: [Integer] -> Int -> IO Integer
primeFailTest l k = do
							x <- sequence [(primeCheck l k) | x <- [1..100]]
							return (minimum x)

leastCompositeFailTest :: Int -> IO Integer
leastCompositeFailTest k = primeFailTest Lab6.composites k

-- the least composite is 9 according to the tests.

-- When we increase k in primeTestsF, we increase the number of random integers we use in
-- Fermat's primality check for k and n. By taking a larger sample size, we perform the check on more
-- numbers, and thus increase the certainty whether n is a prime.

-- We notice that with k = 2, the chance that 9 is the least composite significantly decreases,
-- as we would have to pick 8 twice when selecting a.

--Assignment 5, time 01:25
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

carmichaelFailTest :: Int -> IO Integer
carmichaelFailTest k = primeFailTest carmichael k

-- the primecheck on the carmichael numbers fails at the first carmichael number, 294409.

-- With the composites, we saw different numbers at different runs when trying for false positives.
-- This is because there is a chance that we run into the number a, where a is a random number
-- between 2 and n-1, and the following condition is satisfied:
	 -- a^(n-1) mod n == 1.

-- To confirm the number is a prime, we should check this for every a, not just a sample.
-- Since we use a low k, we check this for only a couple of numbers, and thus there is a chance
-- that the small sample we took satisfies the condition, giving a false positive.

-- With 9, a = 8 satisfies the condtion, even though 9 is not a prime. 
-- With 15, there are multiple candidates.
-- We therefore sometimes continue the test, and sometimes stop at the first false positive.

-- With the carmichael numbers, every coprime of a carmichael number satisfies the condition.
-- The list we use for the carmichael numbers shows 3 prime factors for each carmichael number.
-- Since the numbers are fairly large, the chance that we run into a coprime of the carmichael number
-- while sampling is very small.

-- We therefore usually see the first carmichael number as a false positive.


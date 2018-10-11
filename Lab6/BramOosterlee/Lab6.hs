module Lab6 where

import Data.List
import System.Random
import Lecture6

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

primeFalsePositiveTest :: [Integer] -> Int -> IO Integer
primeFalsePositiveTest l k = do
							x <- sequence [(primeCheck l k) | x <- [1..100]]
							return (minimum x)

leastCompositeFalsePositiveTest :: Int -> IO Integer
leastCompositeFalsePositiveTest k = primeFalsePositiveTest Lab6.composites k

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

carmichaelFalsePositiveTest :: Int -> IO Integer
carmichaelFalsePositiveTest k = primeFalsePositiveTest carmichael k

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

--Assignment 6 time 02:30
primeCheckMR :: [Integer] -> Int -> IO Integer
primeCheckMR (x:xs) k = do
						y <- primeMR k x
						if y 
						then return x 
						else (primeCheck xs k)

primeFalsePositiveTestMR :: [Integer] -> Int -> IO Integer
primeFalsePositiveTestMR l k = do
							x <- sequence [(primeCheckMR l k) | x <- [1..100]]
							return (minimum x)

carmichaelFalsePositiveMRTest :: Int -> IO Integer
carmichaelFalsePositiveMRTest k = primeFalsePositiveTestMR carmichael k

-- With primeCheckMR, we see that the result is 56052361 more often than our first carmichael number
-- 294409, found with the previous exercise.


-- The following implementation will calculate all mersenne primes, given enough time.
-- The issue is that the line {l <- (mersenneSequenceInternal xs k)} turns the IO [Integer] list into
-- an Integer list. This requires the system to first calculate the left over mersenne primes list
-- before continuing, which is never completed.

-- The code therefore never returns a value.
mersenneSequenceInternal :: [Integer] -> Int -> IO [Integer]
mersenneSequenceInternal (x:xs) k = do
										y <- primeMR k (2^x-1)
										if y 
										then do
											l <- (mersenneSequenceInternal xs k)
											return ([2^x-1] ++ l)
										else (mersenneSequenceInternal xs k)

mersenneSequence :: Int -> [IO Integer]
mersenneSequence = mersenneSequenceInternal primes

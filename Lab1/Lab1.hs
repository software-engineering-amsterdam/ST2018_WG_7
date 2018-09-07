module Lab1 where

import Data.List
import Test.QuickCheck


prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

reversal :: Integer -> Integer
reversal = read . reverse . show

-- ASSIGNMENT 1.1 --

sumOfSquares :: Int -> Int
sumOfSquares n = sum [ x^2 | x <- [1..n]]

formalSumOfSquares :: Int -> Int
formalSumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6

sumOfSquaresTest :: (Positive Int) -> Bool
sumOfSquaresTest (Positive n) = sumOfSquares n == formalSumOfSquares n

-- ASSIGNMENT 1.2 --

sumOfQubic :: Int -> Int
sumOfQubic n = sum [ x^3 | x <- [1..n]]

formalSumOfQubic :: Int -> Int
formalSumOfQubic n = (n * (n + 1) `div` 2) ^ 2

sumOfQubicTest :: (Positive Int) -> Bool
sumOfQubicTest (Positive n) = sumOfQubic n == formalSumOfQubic n


-- ASSIGNMENT 2 --

bruteForceCardinality :: Int -> Int
bruteForceCardinality n = length( subsequences[1..n] )

formalCardinality :: Int -> Int
formalCardinality n = 2 ^ n

cardinalityProof :: (Positive Integer) -> Bool
cardinalityProof (Positive n) = bruteForceCardinality n == formalCardinality n

-- Logically the property is simple to test. To do so in a timely manner is hard,
-- as the powerset size supposedly grows exponentially.

-- We're testing whether subsequences creates a number of subsequences corresponding to
-- |P(A)| = 2^n, where n = |A|. In this test we do not know whether all natural numbers until n
-- are created, and it's not just creating 2^n empty subsequences for example.


-- ASSIGNMENT 3 --
cardinality = length

formalNumberOfPermutations :: Int -> Int
formalNumberOfPermutations n = product[1..n]

countedNumberOfPermutations :: Int -> Int
countedNumberOfPermutations n = cardinality(permutations[1..n])

permutationsProof :: (Positive Int) -> Bool
permutationsProof (Positive n) = formalNumberOfPermutations n == countedNumberOfPermutations n

-- We "guess" that the number of possible permutations scales with n!.
-- Logically the property is simple to test. To do so in a timely manner is hard,
-- as the number of possible permutations grows factorially for longer sets.

-- We're testing whether the number of sets returned by permutations(A) corresponds to
-- the factorial of the cardinality of A. We do not check whether the content of the lists
-- returned by permutations() is correct.


-- ASSIGNMENT 4 --

isReversalPrime :: Integer -> Bool
isReversalPrime n = prime (reversal n)

reversalPrimes :: [Integer]
reversalPrimes = [p | p <- (takeWhile (< 10000) primes), isReversalPrime p]
-- This is a function that is very hard to test for completeness. To test the function you
-- would need a property of primes that can be reversed, but we're already using this
-- property to generate the list of reversalPrimes. The test would therefore be another
-- implementation of the function that we're trying to test.

-- Optional: testing the reversal function (as suggested by Ana)
testReversal :: (Positive Integer) -> Bool
testReversal (Positive n) = (reversal (reversal n)) == n
-- The reversal function does not work correctly for integers that end with a zero. For example
-- 30 would be reversed to 03. Since 03 is just 3 it can't be reversed back to 30.


-- ASSIGNMENT 5 --

sumsOfNElements :: Int -> [Integer] -> [Integer]
sumsOfNElements n xs = [sum(take n xs)] ++ sumsOfNElements n (tail xs)

firstPrimeOfSumOf101Primes :: Integer
firstPrimeOfSumOf101Primes = head [p | p <- (sumsOfNElements 101 primes), prime p]
-- Functionally our functions are quite simple and straightforward and need hardly be tested.
-- Mathematically, the first and foremost way to show that the value we find is indeed the lowest
-- prime that is also a sum of 101 consecutive primes is by using our function. Since there is no
-- formal means of finding this number, there is no formal means of testing our function and its result.

-- The function firstPrimeOfSumOf101Primes uses standard Haskell constructs and is straight forward.
-- So the only part that should be checked is the summation of n consecutive numbers, aka the sumsOfNElements function.

-- Testing the functionality of our functions can be done by testing them against the example given in the exercise:
Another way is checking that the example provided in the assignment if present:
firstPrimeOfSumOf5PrimesTest :: Bool
firstPrimeOfSumOf5PrimesTest = any (==101)  (filter prime (sumsOfNElements 5 primes))


-- ASSIGNMENT 6 -- 

conjecture :: Int -> Bool
conjecture n = prime (product (take n primes) + 1)

counterExamples :: [Int]
counterExamples = [n | n <- [1..], not (conjecture n)]

smallestCounterExample :: Int
smallestCounterExample = head counterExamples
-- The smallest counterexample is the list of the first 6 primes: (2*3*5*7*11*13)+1 = 30031

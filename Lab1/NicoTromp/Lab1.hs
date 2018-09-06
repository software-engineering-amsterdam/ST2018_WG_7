
module Lab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]


-- ASSIGNMENT 1.1 --

-- show that 1^2 + 2^2 + 3^2 + ... + n^2 == n(n+1)(2n+1)/6 holds

sumOfSquares :: Int -> Int
sumOfSquares n = sum [ x^2 | x <- [1..n]]

formalSumOfSquares :: Int -> Int
formalSumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6

sumOfSquaresTest :: (Positive Int) -> Bool
--sumOfSquaresTest n = sumOfSquares (abs n) == formalSumOfSquares (abs n)
--sumOfSquaresTest n = n > 0 --> sumOfSquares n == formalSumOfSquares n
sumOfSquaresTest (Positive n) = sumOfSquares n == formalSumOfSquares n
-- The downside of this approach is that you only use (roughly) half of the numbers generated by QuickCheck!

-- 2 hours, including catching up on Haskell (first version in 45 minutes)

-- Show that 1^3 + 2^3 + 3^3 + ... + n^3 == (n(n+1)/2)^2 holds

sumOfQubic :: Int -> Int
sumOfQubic n = sum [ x^3 | x <- [1..n]]

formalSumOfQubic :: Int -> Int
formalSumOfQubic n = (n * (n + 1) `div` 2) ^ 2

sumOfQubicTest :: (Positive Int) -> Bool
--sumOfQubicTest n = sumOfQubic (abs n) == formalSumOfQubic (abs n)
sumOfQubicTest (Positive n) = n > 0 --> sumOfQubic n == formalSumOfQubic n

-- 10 minutes, including changing after swithing to the infix --> operation


-- ASSIGNMENT 1.2 --
-- Power set

powerSetSizeTest :: (Positive Int) -> Bool
powerSetSizeTest (Positive n) = n <= 15 --> length (subsequences [1..n]) == 2^n

-- 40 minutes including discussing with Bert
-- 10 minutes for the initial test
-- + 5 minutes for running and limiting the upper limit
-- Finding this test was not hard. The test itself runs however very long.
-- This is (probably) caused by the fact that QuickCheck generated a wide range of numbers and since the number of subsequences
-- grows exponentially determining the left hand side of the test can take an infinite amount of time.
-- Since the test only tests a very limited number of values it only checks if 'subsequences' satifies the specification.


-- ASSIGNMENT 1.3 --
-- Permutations

fact :: Integral i => i -> i
fact 0 = 1
fact n = n * fact (n - 1)

permutationsSizeTest :: (Positive Int) -> Bool
permutationsSizeTest (Positive n) = n <= 10 --> length (permutations [1..n]) == fact n

-- Once again this test is way to slow to be practical. So in the end we test only if the specification is partially satiafied.

-- 20 minutes


-- ASSIGNMENT 1.4 --

isReversalPrime :: Integer -> Bool
isReversalPrime n = prime (reversal n)

reversalPrimes :: [Integer]
reversalPrimes = filter isReversalPrime (takeWhile (< 10000) primes)

-- 15 minutes writing the reversalPrimes function
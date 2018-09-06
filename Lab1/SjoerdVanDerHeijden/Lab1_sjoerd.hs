{- this is a comment -}
module Lab1_sjoerd where
import Data.List
import Test.QuickCheck

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

primes :: [Integer]
primes = 2 : filter isPrime [3..]

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
  
reversal :: Integer -> Integer
reversal = read . reverse . show

-- source: https://wiki.haskell.org/Prime_numbers
primesTo :: Integer -> [Integer]
primesTo m = sieve [2..m]
             where 
             sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
             sieve [] = []

-------------------------------------------------------------------------
-- Exercise 1
mySquareSumProof :: Integer -> Bool
mySquareSumProof n = (n > 0) --> (sum(map(^2)[1..n]) == (n*(n+1)*((2*n)+1)) `div` 6 )

mySquareSumProof2 :: (Positive Integer) -> Bool
mySquareSumProof2 (Positive n) = (sum(map(^2)[1..n]) == (n*(n+1)*((2*n)+1)) `div` 6 )

myCubeSumProof :: (Positive Integer) -> Bool
myCubeSumProof (Positive n) = (sum(map(^3)[1..n]) == (n*(n+1) `div` 2 )^2)

--time: 2h
-------------------------------------------------------------------------
--Exercise 2
myCardinalityProof :: (Positive Integer) -> Bool
myCardinalityProof (Positive n) = (2^n) == length(subsequences[1..n])

{-This property is logically easy to check, but computationally difficult; I eventually stopped quickCheck at 39 tests.
I'm testing whether the proposition given in workshop1.4 holds true, assuming that the subsequences function
works as promised. -}

--time: 40m
-------------------------------------------------------------------------
--Exercise 3
myPermutationsProof :: (Positive Int) -> Bool
myPermutationsProof (Positive n) = product[1..n] == length(permutations[1..n])
{-Again, this is logically not hard, but computationally it is, due to the exponentially
growing required computations for linearly growing numbers.
Again I'm testing a proposition, while assuming correct functionality of my tools. -}

--time 20m
-------------------------------------------------------------------------
--Exercise 4
primesEx4 = primesTo 10000

myReversiblePrimes :: [Integer]
myReversiblePrimes = [x | x <- primesEx4, elem (reversal x) primesEx4]

{-I'd test this by checking my results with an existing and verified list of reversible primes -}
--time:30m
-------------------------------------------------------------------------
--Exercise 5
primesEx5 = primesTo 10000

mySumPrime :: Integer
mySumPrime = head [sum(take 101 (drop x primesEx5 ))| x <- [0..], isPrime(sum(take 101 (drop x primesEx5 ))) ]

{-Since the question is only to find the smallest one, mySumPrime needs only be tested for doing what it is
meant to do. It need not be mathematically proven. -}
--time:10m
-------------------------------------------------------------------------
--Exercise 6

myPrimeConjecture :: Int -> Bool
myPrimeConjecture n = isPrime( product(take n primes)+1 )

myMinimumPrimeConjectureDisprove = head [x | x <- [1..], not (myPrimeConjecture x)]

{- 5 -}
--time:30m
-------------------------------------------------------------------------
--Exercise 7
luhn :: Integer -> Bool
luhnstep1 n = [ | x <- show(n)]
[x | x <- read luhnstep1 :: []]

for i in str(code):
        


























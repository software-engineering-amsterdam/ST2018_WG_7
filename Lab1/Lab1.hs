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

runSumOfSquaresTest = quickCheck myCubeSumProof

-- ASSIGNMENT 1.2 --

sumOfQubic :: Int -> Int
sumOfQubic n = sum [ x^3 | x <- [1..n]]

formalSumOfQubic :: Int -> Int
formalSumOfQubic n = (n * (n + 1) `div` 2) ^ 2

sumOfQubicTest :: (Positive Int) -> Bool
sumOfQubicTest (Positive n) = sumOfQubic n == formalSumOfQubic n

runSumOfQubicTest = quickCheck sumOfQubicTest

-- ASSIGNMENT 2 --

bruteForceCardinality :: Int -> Int
bruteForceCardinality n = length( subsequences[1..n] )

formalCardinality :: Int -> Int
formalCardinality n = 2 ^ n

cardinalityProof :: (Positive Int) -> Bool
cardinalityProof (Positive n) = bruteForceCardinality n == formalCardinality n

runCardinalityProof = quickCheck cardinalityProof
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

runPermutationsProof = quickCheck permutationsProof

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

runReversalPrimes = reversalPrimes

runTestReversal = quickCheck testReversal

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
-- Another way is checking that the example provided in the assignment if present:
firstPrimeOfSumOf5PrimesTest :: Bool
firstPrimeOfSumOf5PrimesTest = any (==101)  (filter prime (sumsOfNElements 5 primes))

runFirstPrimeOfSumOf101Primes = firstPrimeOfSumOf101Primes

-- ASSIGNMENT 6 -- 
conjecture :: Int -> Bool
conjecture n = prime (product (take n primes) + 1)

counterExamples :: [Int]
counterExamples = [n | n <- [1..], not (conjecture n)]

smallestCounterExample :: Int
smallestCounterExample = head counterExamples
-- The smallest counterexample is the list of the first 6 primes: (2*3*5*7*11*13)+1 = 30031
runSmallestCounterExample = smallestCounterExample


-- ASSIGNMENT 7 -- 
luhnDouble :: Integer -> Integer
luhnDouble n | x < 10    = x
             | otherwise = x - 9
           where x = 2 * n

luhnify :: Integer -> Integer
luhnify x | x < 10    = x
          | x < 100   = mod x 10 + luhnDouble (x `div` 10)
          | otherwise = luhnify (mod x 100) + luhnify (x `div` 100)

isValidLuhn :: Integer -> Bool
isValidLuhn n = mod (luhnify n) 10 == 0

isValidLength :: Int -> Integer -> Bool
isValidLength n x = x >= 10^(n-1) && x < 10^n

-- Using show is more accurate then logBase 10 since that returns a number and I have seen cases
-- where it was rounded up while it should not have been rounded up!
fitToLengthOfMask :: Integer -> Integer -> Integer
fitToLengthOfMask n m = n `div` (10 ^ (length (show n) - length (show m)))

startsWithAny :: Integer -> [Integer] -> Bool
startsWithAny n ms = elem (fitToLengthOfMask n (head ms)) ms

americanExpresIINs = [34, 36]
isAmericanExpress :: Integer -> Bool
isAmericanExpress x = isValidLuhn x && isValidLength 15 x && startsWithAny x americanExpresIINs

masterCardIINs = [51..55] ++ [2221..2720]
isMastercard :: Integer -> Bool
isMastercard x = isValidLuhn x && isValidLength 16 x && startsWithAny x masterCardIINs

visaIINs = [4]
isVisa :: Integer -> Bool
isVisa x = isValidLuhn x && isValidLength 16 x && startsWithAny x visaIINs

-- Test:
-- Check that a creditcard number is valid for a single company. Credit card numbers that
-- are valid for one company should be invalid for the other companies.
-- The problem with testing our code is that we should not use this code when generating 
-- credit card numbers.
-- We can however search for a number of valid creditcard numbers using the internet and
-- generaring different valid numbers out of them using the following trick.
-- Assume creditcard numbers take the following form: iiixyxyxyxyxyxyc
-- Where iii is the indentification number, c is the check digit and x and x are the remainder
-- of the credit card number. We can replace all the y's and x's with any permutation of the
-- y values and the x values respectively. This would result for Visa in 7! * 7! (=25401600)
-- and for mastercard to minium of 6! * 5! (=86400) different numbers.
-- The numbers that are used in the test where taken from https://www.freeformatter.com/credit-card-number-generator-validator.html#fakeNumbers
-- Please keep in mind that on every load of the page new numbers are generated, so the
-- change that the page will show the numbers used in the tests is very slim.

isValidLuhnTest :: Bool
isValidLuhnTest = isValidLuhn 79927398713

data CCInfo = CCInfo { iin :: [Integer]
                     , doubles :: [Integer]
                     , singles :: [Integer]
                     , check :: Integer
                     } deriving (Show)

toDigits :: Integer -> [Integer]
toDigits x | x < 10    = [x]
           | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
             where addDigit num d = 10*num + d

getDoubles :: [Integer] -> [Integer]
getDoubles []       = []
getDoubles (x:y:zs) = [x] ++ getDoubles zs
getDoubles [x]      = [x]

getSingles :: [Integer] -> [Integer]
getSingles []       = []
getSingles (x:y:zs) = [y] ++ getSingles zs
getSingles [x]      = []

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) = [x] ++ [y] ++ (merge xs ys)

toCCInfo :: Integer -> Integer -> Integer -> CCInfo
toCCInfo iin an check = CCInfo { iin = toDigits iin
                         , doubles = reverse (getDoubles (reverse (toDigits an)))
                         , singles = reverse (getSingles (reverse (toDigits an)))
                         , check = check}

fromCCInfo :: CCInfo -> Integer
fromCCInfo (CCInfo iin doubles singles c) = fromDigits (iin ++ reverse (merge (reverse doubles) (reverse singles)) ++ [c])

generatePermutations :: CCInfo -> [Integer]
generatePermutations (CCInfo iin doubles singles c) = [ fromCCInfo (CCInfo iin ds ss c) | ds <- permutations doubles, ss <- permutations singles ] 

validate :: Integer -> (Integer -> Bool) -> [(Integer -> Bool)] -> Bool
validate x v fs = v x && not (or [ f x | f <- fs ])

isValidAmericanExpress :: Integer -> Bool
isValidAmericanExpress x = validate x isAmericanExpress [isMastercard, isVisa]

isValidMastercard :: Integer -> Bool
isValidMastercard x = validate x isMastercard [isAmericanExpress, isVisa]

isValidVisa :: Integer -> Bool
isValidVisa x = validate x isVisa [isAmericanExpress, isMastercard]

testVisaCards = and (take 20000 [ isValidVisa x | x <- generatePermutations (toCCInfo 4 48520577603243 6)])
testMasterCards = and (take 20000 [ isValidMastercard x | x <- generatePermutations (toCCInfo 52 4717353806721 3)])
testAmericanExpressCards = and (take 20000 [ isValidAmericanExpress x | x <- generatePermutations (toCCInfo 34 904591690693 1)])

-- 1 hour, rough version
-- 30 minutes refactoring IIN identification
-- 20 minutes adding validation per company
-- 2 hours implementing tests


-- == PROJECT EULER == --
-- PROBLEM 9 --
pythagoreanWithCircumference :: Int -> [[Int]]
pythagoreanWithCircumference n = [ [a, b, n - a - b] | a <- [1..n], b <- [(a+1)..n], a^2 + b^2 == (n - a - b)^2 ]

specialPythagoreanValues :: [Int]
specialPythagoreanValues = head (pythagoreanWithCircumference 1000)

specialPythagoreanProduct :: Int
specialPythagoreanProduct = product specialPythagoreanValues
-- 25 minutes, including generalisation

-- PROBLEM 10 --
euler2 :: Integer
euler2 = sum[x | x <- takeWhile (< 2*10^6) primes]
--time: 20min

-- PROBLEM 49 --
fourDigitPrimes :: [Int]
fourDigitPrimes = [ n | n <- [1001..9997], prime (toInteger n)]

isPermutaton :: Int -> Int -> Bool
isPermutaton x y = sort (show x) == sort (show y)

findPermutations :: Int -> [Int] -> [Int]
findPermutations p xs = [ x | x <- xs, isPermutaton p x]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

primePermutations :: [Int] -> [[Int]]
primePermutations xs = rmdups [ findPermutations x xs | x <- xs]

specialPrimeCandidates :: [[Int]]
specialPrimeCandidates = filter (\x -> length x >= 3) (primePermutations fourDigitPrimes)

specialPrimeCandidatesTriplets :: [[Int]]
specialPrimeCandidatesTriplets = [ ys | xs <- specialPrimeCandidates, ys <- subsequences xs, length ys == 3]

isSpecialPrimes :: [Int] -> Bool
isSpecialPrimes (x:y:zs) = 2*y - x == head zs

specialPrimes :: [[Int]]
specialPrimes = filter isSpecialPrimes specialPrimeCandidatesTriplets
-- 4 hours





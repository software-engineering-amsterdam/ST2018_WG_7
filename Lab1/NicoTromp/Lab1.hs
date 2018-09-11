
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

factorial :: Integral i => i -> i
factorial 0 = 1
factorial n = n * factorial (n - 1)

permutationsSizeTest :: (Positive Int) -> Bool
permutationsSizeTest (Positive n) = n <= 10 --> length (permutations [1..n]) == factorial n

-- Once again this test is way to slow to be practical. So in the end we test only if the specification is partially satiafied.

-- 20 minutes


-- ASSIGNMENT 1.4 --

isReversalPrime :: Integer -> Bool
isReversalPrime n = prime (reversal n)

reversalPrimes :: [Integer]
reversalPrimes = filter isReversalPrime (takeWhile (< 10000) primes)

-- 15 minutes writing the reversalPrimes function
-- 

-- ASSIGNMENT 1.5 --

sumsOfNElements :: Int -> [Integer] -> [Integer]
sumsOfNElements n xs = [sum(take n xs)] ++ sumsOfNElements n (tail xs)

firstPrimeOfSumOf101Primes :: Integer
firstPrimeOfSumOf101Primes = head (filter prime (sumsOfNElements 101 primes))

-- 1 hour including discussing with teammates
-- The function firstPrimeOfSumOf101Primes uses standard Haskell constructs and is straight forward.
-- So the only part that should be checked is the summation of n consequtive numers, aka the sumsOfNElements function.
-- Another way is checking that the example provided in the assignment if present.

firstPrimeOfSumOf5PrimesTest :: Bool
firstPrimeOfSumOf5PrimesTest = any (==101) (filter prime (sumsOfNElements 5 primes))

-- ASSIGNMENT 1.6 --

productOfNElements :: Int -> [Integer] -> Integer
productOfNElements n xs = product(take n xs) + 1

counterExamplesPrimesProductIsPrime :: [Int]
counterExamplesPrimesProductIsPrime = [n | n <- [1..], not (prime (productOfNElements n primes))]

smallesCounterExample :: Int
smallesCounterExample = head counterExamplesPrimesProductIsPrime

-- ASSIGNMENT 1.7 --

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
startsWithAny n ms = or [ fitToLengthOfMask n m == m | m <- ms]

americanExpresIINs = [34, 36]
isAmericanExpress :: Integer -> Bool
isAmericanExpress x = isValidLuhn x && isValidLength 15 x && startsWithAny x americanExpresIINs

masterCardIINs = [51..55] ++ [2221..2720]
isMastercard :: Integer -> Bool
isMastercard x = isValidLuhn x && isValidLength 16 x && startsWithAny x masterCardIINs

visaIINs = [4]
isVisa :: Integer -> Bool
isVisa x = isValidLuhn x && isValidLength 16 x && startsWithAny x visaIINs

-- Test

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

-- ASSIGNMENT 1.8 --

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

-- Set like equals operators that are not sensitive for the order of the elements in the list
equals :: (Eq a) => [a] -> [a] -> Bool
equals xs ys = length xs == length ys && and [ elem x ys | x <- xs]

notEquals :: (Eq a) => [a] -> [a] -> Bool
notEquals xs ys = not (equals xs ys)


accuses :: Boy -> Boy -> Bool
accuses Matthew accused = not (elem accused [Carl, Matthew])
accuses Peter accused   = elem accused [Matthew, Jack]
accuses Jack accused    = not (accuses Matthew accused) && not (accuses Peter accused)
accuses Arnold accused  = xor (accuses Matthew accused) (accuses Peter accused)
accuses Carl accused    = not (accuses Arnold accused)

accusers :: Boy -> [Boy]
accusers accusee = [b | b <- boys, accuses b accusee]

-- The following two function capture the knowledge of the teacher
-- Three boys are telling the truth.
potentialAngels :: [[Boy]]
potentialAngels = filter (\ bs -> length bs == 3) (subsequences boys)

-- Two boys are lying given the possible angels
potentialLiers :: [Boy] -> [Boy]
potentialLiers ys = boys \\ ys

truthfullyAccusedBy :: Boy -> [Boy] -> Bool
truthfullyAccusedBy c ys = accusers c `equals` ys

falselyAccusedBy :: Boy -> [Boy] -> Bool
falselyAccusedBy c xs = accusers c `notEquals` xs

judge :: [(Boy, [Boy])]
judge = [ (c, ys) | c <- boys, ys <- potentialAngels, truthfullyAccusedBy c ys && falselyAccusedBy c (potentialLiers ys)]

guilty :: [Boy]
guilty = [fst (head judge)]

honest :: [Boy]
honest = snd (head judge)

-- 3 hours. 


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
sumOfPrimes :: Integer
sumOfPrimes = sum (filter prime [2..2000000])

-- 15 minutes

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
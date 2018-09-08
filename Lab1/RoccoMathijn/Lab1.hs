module Lab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

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

--exercise 1: ~30 minutes
sumToN :: Int -> Int
sumToN n = n * (n + 1) `div` 2

sumToNTest :: (Positive Int) -> Bool
sumToNTest (Positive n) = sumToN n == sum [1..n]

sumToNToThePower :: Int -> Int
sumToNToThePower n = (n * (n + 1) * (2 * n + 1)) `div` 6

sumToNToThePowerTest :: (Positive Int) -> Bool
sumToNToThePowerTest (Positive n) = sumToNToThePower n == sum [n^2  | n <- [1..n]]

sumToNToThePower3 :: Int -> Int
sumToNToThePower3 n = ((n * (n + 1)) `div` 2)^2

sumToNToThePower3Test :: (Positive Int) -> Bool
sumToNToThePower3Test (Positive n) = sumToNToThePower3 n == sum [n^3  | n <- [1..n]]

--exercise 2: ~15 minutes
exercise2Test :: (Positive Int) -> Bool
exercise2Test (Positive n) =  2 ^ y == length (subsequences [1..n]) where
                              y = length [1..n]
--We're actually not testing the mathematical fact that |powersetA| = 2^|A|. 
--Actually we're testing that the subsequences function returns a set that has 2^|A| elements.

--exercise 3: ~10 minutes
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

exercise3Test :: (Positive Int) -> Bool
exercise3Test (Positive n) = length (permutations [1..n]) == factorial n
--Here we're actually not testing that the number of possibble permutations equals our factorial function. We're testing 
-- if the function permutations returns the same number as our factorial function

--exercise 4: ~20 minutes
reversalPrimes :: [Integer]
reversalPrimes = [p | p <- takeWhile (<10000) primes, prime p && prime (reversal p)]

testReversal :: (Positive Integer) -> Bool
testReversal (Positive n) = (reversal (reversal n)) == n
-- The reversal function does not work well on integers that end in a zero. For example 30 would be reversed to 03. 
-- Since 03 is just 3 it can't be reversed back to 30.


-- This is a function that is very hard to test. To test the function you would need a property of primes that can be reversed
-- But we're already using this property to generate the list of reversalPrimes. The test would therefore be another implementation
-- of the function that we're trying to test.

--exercise 5: ~25 minutes
slice :: Int -> Int -> [Integer] -> [Integer]
slice from to xs = take (to - from + 1) (drop from xs)

consecutivePrimes :: [[Integer]]
consecutivePrimes = [slice n (n+100) primes | n <-[1..]]

firstPrimeSum :: Integer
firstPrimeSum = head [sumPrimes | sumPrimes <- map sum consecutivePrimes, prime sumPrimes]

-- Also for this exercise the test would entail another implementation of the function that we're trying to test.
-- We can maybe check if the result is a prime but we already check that in the implementation of the function.

--exercise 6: ~15 minutes
conjecture :: Int -> Bool
conjecture n = prime (product (take n primes) + 1)

counterExamples :: [Int]
counterExamples = [n | n <- [1..], not (conjecture n)]

smallestCounterExample :: Int
smallestCounterExample = head counterExamples


--exercise 7: 75 minutes
luhnDouble :: Integer -> Integer
luhnDouble i =  if res > 9 then res - 9 else res
                where res = i * 2

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f []        = []
mapEveryOther f (x:[])    = [x]
mapEveryOther f (x:x2:xs) = x : (f x2) : mapEveryOther f xs 

numberToList :: Integer -> [Integer]
numberToList n = [read [c] | c <- show n]

luhn :: Integer -> Bool
luhn n =  if sumDigits `mod` 10 == 0 then True else False
          where sumDigits = sum luhnDoubled
                numberInList = numberToList n
                luhnDoubled = mapEveryOther luhnDouble (reverse numberInList)

firstNDigits :: Int -> Integer -> Integer
firstNDigits n digits = read firstNAsString
                        where firstNAsString = foldr (++) "" [show d | d <- listOfDigits]
                              listOfDigits = take n (numberToList digits)

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n && 
                      numberOfDigits == 15 && 
                      (firstTwo == 34 || firstTwo == 37)
                      where firstTwo = firstNDigits 2 n
                            numberOfDigits = length (show n)

isMaster :: Integer -> Bool
isMaster n = luhn n &&
             numberOfDigits == 16 &&
             ((firstFour > 5100 && firstFour < 5500) || (firstFour > 2221 && firstFour < 2720))
             where firstFour = firstNDigits 4 n
                   numberOfDigits = length (show n)

isVisa :: Integer -> Bool
isVisa n =  luhn n &&
            numberOfDigits == 16 &&
            first == 4
            where first = firstNDigits 1 n
                  numberOfDigits = length (show n)


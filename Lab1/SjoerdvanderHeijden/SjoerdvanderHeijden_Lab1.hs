{- this is a comment -}
module Lab1_sjoerd where
import Data.List
import Test.QuickCheck
import Data.Numbers.Primes

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--primes :: [Integer]
--primes = 2 : filter isPrime [3..]

--isPrime :: Integer -> Bool
--isPrime n = n > 1 && all (\ x -> rem n x /= 0) xs
  --where xs = takeWhile (\ y -> y^2 <= n) primes
  
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

{- the counterexample is n=6, giving (2*3*5*7*11*13)+1 = 30031, which is not a prime. -}
--time:30m
-------------------------------------------------------------------------
--Exercise 7

-- Splits up a number into a list of single digits; 2018 becomes [2,0,1,8]
intToDigitList :: Int -> [Int]
intToDigitList n = [ read[ i ] :: Int | i <- show(n) ]

-- Takes first takeN digits of an int and turns it into and int again; takeFirstFromInt 2 2018 becomes 20
takeFirstFromInt :: Int -> Int -> Int
takeFirstFromInt takeN n = read(take takeN (show(n))) :: Int

-- Returns the different lengths (in terms of number of digits) of the entries of a list of ints, for use with checkIIN
intLengthsInList :: [Int] -> [Int]
intLengthsInList list = nub( map length (map (show) list))

-- Returns the input list, but with every second entry being doubled, according to Luhn's algorithm.
doubledCodeNumbers :: [Int] -> [Int]
--doubledCodeNumbers digitList = [ if (mod i 2 == mod (length digitList) 2) then (2* (digitList!!i)) else (digitList!!i) | i <- [0..length(digitList)-1]]
doubledCodeNumbers digitList = [ if mod i 2 /= mod (length digitList) 2 then 2* digitList!!i else digitList!!i | i <- [0..length(digitList)-1]]

-- Reduces numbers in the input list if they are greater than 10, according to Luhn's algorithm.
reducedDoubledCodeNumbers :: [Int] -> [Int]
reducedDoubledCodeNumbers doubledCodeNumbers = [if x > 9 then x-9 else x | x <- doubledCodeNumbers]

-- Final check to see whether a number is valid according to Luhn's algorithm
checkLuhn :: Int -> Bool
checkLuhn n = mod(sum(reducedDoubledCodeNumbers(doubledCodeNumbers(intToDigitList(n))))) 10 == 0
-- The above stuff just works, alright? Get off my back about the way it looks
-- time: 2h


-- Checks whether the first digits of a number are in checkIDs
checkIIN :: Int -> [Int] -> Bool
checkIIN n checkIDs = or [elem x (checkIDs) | y <- intLengthsInList checkIDs, x <- [takeFirstFromInt y n]]

checkNAmEx n = checkIIN n [34,37] && length (intToDigitList n) == 15 && checkLuhn n
checkNMC n = checkIIN n ([51..55]++[2221..2720]) && length (intToDigitList n) == 16 && checkLuhn n
checkNVisa n = checkIIN n [4] && length (intToDigitList n) == 16 && checkLuhn n

-- time for this bit: 1h
-------------------------------------------------------------------------
-- Exercise 8

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Peter Matthew = True
accuses Peter Jack = True
accuses _ _ = False

accusers :: Boy -> [Boy]
accusers Matthew = [Peter]
accusers Jack = [Peter]

{- answers I derived myself: 
Matthew honest innocent
Peter honest innocent
Jack liar guilty
Arnold liar innocent
Carl honest innocent
-}
-- Not finished

-------------------------------------------------------------------------
-- Exercise Euler 

euler1 :: [Int]
euler1 = head [[a,b,c] | a <- [1..333], c <- [a+2..997], b <-[a..c], a+b+c==1000, a^2+b^2==c^2]
--time: 10min

euler2 :: Integer
euler2 = sum[x | x <- takeWhile (< 2*10^6) primes]
--time: 20min

--euler3 :: Int
fourDigitPrimes :: [Int]
fourDigitPrimes = [x | x <- takeWhile (< 10^4) primes, x > 999]

--permsOfN n = [[] | x <- fourDigitPrimes, 
--[x | x <- takeWhile (< 10^4) primes, x > 999, not elem False ( map read (permutations (show x)) :: [Int] ) ]







module Lab6 where

import Data.List
import System.Random
import Lecture6

-- EXERCISE 1 --

-- See Lecture1.hs
-- exM :: Integer -> Integer -> Integer -> Integer
-- exM x p n | p == 0    = 1
--           | even p    = xhp^2 `mod` n
--           | otherwise = (xm * xhp^2) `mod` n
--           where xm = x `mod` n
--                 xhp = exM xm (p `div` 2) n


-- EXERCISE 2 --

{-
Within GHCI, the REPL that I use, it is possible to instruct it to show
the time and memory consumption of each function.

Below is the output of two runs with expM and exM.

*Lab6> expM 9999999 999999 53425021
7655597
(0.20 secs, 8,339,632 bytes)
*Lab6> 

*Lab6> exM 9999999 999999 53425021
7655597
(0.00 secs, 102,336 bytes)
*Lab6>

This clearly shows that exM is both more efficient time- and memory wise.
This can be explained by the fact that Haskell doesn't use the CPUs native 
integer data types but its own internal representation. Using this internal
representation Haskell is capable of performing calulations with numbers
of almost infinite length. Basically Haskell mimics the way a human would 
calculate this. The result is a very large number that will be more or less
linear with the number of digits. Doing calculations with these very big numbers
takes more time and memory then when the values stay small.
Our optimized exM function ensures that the numbers Haskell has to do the
calculation on stay small. Hence the optimized form is more efficient both time-
and memorywise.
-}


-- EXERCISE 3 --

-- See Lecture1.hs
-- composites :: [Integer]
-- composites = [ n | n <- [2..], any (\a -> exM a (n-1) n /= 1) [2..(n-1)]]


-- EXERCISE 4 --

-- -- Tests the Fermat Primality Test, it prints the first composite number for which
-- -- the primality check fails.
findFirstFailingComposite :: Int -> [Integer] -> IO Integer
findFirstFailingComposite k (n:ns) = do v <- primeTestsF k n
                                        if v then return n
                                        else findFirstFailingComposite k ns

testFermatPrimalityTest k = findFirstFailingComposite k composites

-- See Bram, we did it together :-)


-- EXERCISE 5 --

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | k <- [2..], prime (6*k+1), prime (12*k+1), prime (18*k+1) ]

firstCarmichael = head carmichael

{-
A Carmichael number is a multiplication of three prime numbers. Becuse it is made of three prime numbers there are
no other factors then the three original prime numbers. Because these factors are prime numbers the Fermat
primality test will not recognize it as a factor. Since there are only three factors the change that the primality
test will select them at random. As long as k is small almost all the time a non-factor is selected. This will
result in a false positive.
The first Carmichael number the code above generates uses 37, 73 and 109 as the factors
-}

-- EXERCISE 6.1 --

findFirstFailingMR :: Int -> [Integer] -> IO Integer
findFirstFailingMR k (n:ns) = do v <- primeMR k n
                                 if v then return n
                                 else findFirstFailingMR k ns

testMRPrimalityTest k = findFirstFailingMR k carmichael

{-
Using the code above it is possible to get false positves as long as k is kept small.
The bigger k is the bigger the first Carmichael number is that fails the test.
-}


-- EXERCISE 6.2 --

isPrime :: Integer -> IO (Bool, Integer)
isPrime n = do v <- primeMR 10 n
               return (v,n)

mersennePrimes :: Integer -> IO [Integer]
mersennePrimes p = do ps <- sequence $ [ isPrime (2^p' - 1) | p' <- [2..p], prime p' ]
                      return (map snd (filter fst ps))

showPrimes :: [Integer] -> IO ()
showPrimes []     = putStrLn "That's all folks..."
showPrimes (p:ps) = do putStrLn ("MP = " ++ (show p))
                       showPrimes ps

main :: Integer -> IO ()
main m = do ps <- mersennePrimes m
            showPrimes ps


    

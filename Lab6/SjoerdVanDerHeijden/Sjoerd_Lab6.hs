module Sjoerd_Lab6 where

import Data.List
import Test.QuickCheck
import Data.Time.Clock

import Lecture6

-------------------------------------------------------------------------------
-- helper code

quicktester :: Integral a => a -> Bool 
quicktester a = ((mod (a^32) 17) * (mod a 17)) == (mod (a^33) 17)

exmTester :: Integer -> Integer -> Integer -> Bool
exmTester x power divisor | divisor /= 0 && power > -1 = exM x power divisor == expM x power divisor
                          | otherwise = True

-------------------------------------------------------------------------------
-- == Assignment 1 == --
-- exM :: Integer -> Integer -> Integer -> Integer
-- exM x 0 divisor = 1
-- exM x power divisor |even power = (exM x (power `div` 2) divisor)^2 `mod` divisor
--                     | otherwise = (exM x (power-1) divisor * x) `mod` divisor

-------------------------------------------------------------------------------
-- == Assignment 2 == --

{-
I like it well enough now:
*Sjoerd_Lab6> :set +s
*Sjoerd_Lab6> expM 9999999 999999 53425021
7655597
(0.41 secs, 8,308,576 bytes)
*Sjoerd_Lab6> exM 9999999 999999 53425021
7655597
(0.02 secs, 76,352 bytes)

This shows that my exM function is a lot more efficient than the old exM function.
-}

-------------------------------------------------------------------------------
-- == Assignment 3 == --


-- composites :: [Integer]
-- composites = [n | n <- [1..], any (\x -> 0 == n `mod` x) [2..(div n 3)] ]


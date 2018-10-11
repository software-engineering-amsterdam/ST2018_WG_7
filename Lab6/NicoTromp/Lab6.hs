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

*Lab6> expM 9999999 999999 5
4
(0.21 secs, 8,334,496 bytes)
*Lab6>

*Lab6> exM 9999999 999999 5
4
(0.00 secs, 98,080 bytes)
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
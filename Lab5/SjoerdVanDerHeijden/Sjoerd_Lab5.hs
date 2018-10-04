module Sjoerd_Lab5 where

import Data.List
import Test.QuickCheck

-- import Lecture5
import Sjoerd_Lab5_Ex1
import Sjoerd_Lab5_Ex2

-------------------------------------------------------------------------------
-- Helper code

x = Sjoerd_Lab5_Ex1.grid2sud exercise1

exercise1 = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]


-------------------------------------------------------------------------------
-- == Assignment 1 == --

execEx1 = do
    Sjoerd_Lab5_Ex1.solveAndShow exercise1

-- Solution:
-- +-------+-------+-------+
-- | 4 7 8 | 3 9 2 | 6 1 5 |
-- | 6 1 9 | 7 5 8 | 3 2 4 |
-- | 2 3 5 | 4 1 6 | 9 7 8 |
-- +-------+-------+-------+
-- | 7 2 6 | 8 3 5 | 1 4 9 |
-- | 8 9 1 | 6 2 4 | 7 5 3 |
-- | 3 5 4 | 9 7 1 | 2 8 6 |
-- +-------+-------+-------+
-- | 5 6 7 | 2 8 9 | 4 3 1 |
-- | 9 8 3 | 1 4 7 | 5 6 2 |
-- | 1 4 2 | 5 6 3 | 8 9 7 |
-- +-------+-------+-------+

-- Time: 2h, would've been less if the Lecture5 code'd been documented

-------------------------------------------------------------------------------
-- == Assignment 2 == --

execEx2 = do
    Sjoerd_Lab5_Ex2.solveAndShow exercise1


main = do
    putStrLn "-- == Assignment 1 == --" 
    execEx1
    putStrLn "-- == Assignment 2 == --" 
    execEx2
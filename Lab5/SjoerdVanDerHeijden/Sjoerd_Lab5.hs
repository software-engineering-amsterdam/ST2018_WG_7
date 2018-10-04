module Sjoerd_Lab5 where

import Data.List
import Test.QuickCheck

-- import Lecture5'
import Sjoerd_Lab5_Ex1
import Sjoerd_Lab5_Ex2
import Sjoerd_Lab5_Ex3_v2
import Sjoerd_Lab5_Ex5

-------------------------------------------------------------------------------
-- Helper code

x = Sjoerd_Lab5_Ex1.grid2sud exercise1

exercise1 :: Sjoerd_Lab5_Ex1.Grid
exercise1 = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

non_min_sud :: Sjoerd_Lab5_Ex1.Grid
non_min_sud = [[0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0]]


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


-------------------------------------------------------------------------------
-- == Assignment 3 == --
execEx3 = do
    x <- minimalTester
    print x

-------------------------------------------------------------------------------
-- == Assignment 5 == --
execEx5 = do
    copyOfMain



main = do
    putStrLn "\n-- == Assignment 1 == --" 
    execEx1
    putStrLn "\n-- == Assignment 2 == --" 
    execEx2
    putStrLn "\n-- == Assignment 3 == --" 
    putStrLn "Please wait...\nIs a randomly generated sudoku minimal? " 
    execEx3
    putStrLn "\n-- == Assignment 5 == --\nGenerating NRC conform sudoku:" 
    copyOfMain
module Lab5 where

import Data.List
import System.Random

import Lab5_Ex1
import Lab5_Ex2
import Lab5_Ex3
import Lab5_Ex4
import Lab5_Ex5

-------------------------------------------------------------------------------
-- == Assignment 1 == --

execEx1 = do
    main1

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
-- see Lab5_Ex1 for code

-------------------------------------------------------------------------------
-- == Assignment 2 == --
execEx2 = do
    main2

-------------------------------------------------------------------------------
-- == Assignment 3 == --
execEx3 = do
    runExercise3

-------------------------------------------------------------------------------
-- == Assignment 4 == --
execEx4 = do
    runExercise4

-------------------------------------------------------------------------------
-- == Assignment 5 == --
execEx5 = do
    main5

-------------------------------------------------------------------------------
-- == Main == --
main = do
    putStrLn "\n-- == Assignment 1 == --" 
    execEx1
    putStrLn "\n-- == Assignment 2 == --" 
    execEx2
    -- putStrLn "\n-- == Assignment 3 == --" 
    -- putStrLn "Please wait...\nIs a randomly generated sudoku minimal? " 
    execEx3
    putStrLn "\n-- == Assignment 4 == --\nGenerating sudoku with 3 empty squares:" 
    execEx4
    putStrLn "\n-- == Assignment 5 == --\nGenerating NRC conform sudoku:" 
    execEx5
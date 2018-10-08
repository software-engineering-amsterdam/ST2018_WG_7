module Lab5EmptyBlocks where

import Data.List

import Lecture5

{-
Select randomly the blocks to empty and then generate a Sudoku problem with the cleaned Sudoku.

Time spent: 0:20
-}

cleanCells :: Node -> [(Row, Column)] -> Node
cleanCells n []     = n
cleanCells n (x:xs) = eraseN (cleanCells n xs) x

cleanBlocks :: Node -> [(Row, Column)] -> Node
cleanBlocks n xs = cleanCells n [ (r',c') | (r,c) <- xs, r' <- blocks !! r, c' <- blocks !! c]

selectEmptyBlocks :: Int -> (Row, Column)
selectEmptyBlocks n = (n `div` 3, n `rem` 3)

genEmptyBlockSudoku :: Int -> IO ()
genEmptyBlockSudoku n = do [r] <- rsolveNs [emptyN]
                           showNode r
                           bs <- randomize [0..8]
                           s  <- genProblem (cleanBlocks r [ selectEmptyBlocks b | b <- take n bs])
                           showNode s

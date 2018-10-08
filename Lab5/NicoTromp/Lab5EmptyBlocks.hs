module Lab5EmptyBlocks where

import Data.List

import Lecture5

{-
Select randomly the blocks to empty and then generate a Sudoku problem with the cleaned Sudoku.

Time spent: 0:25
-}

cleanCells :: Node -> [(Row, Column)] -> Node
cleanCells n []     = n
cleanCells n (x:xs) = eraseN (cleanCells n xs) x

cleanBlocks :: Node -> [Int] -> Node
cleanBlocks n xs = cleanCells n (concatMap (\x -> [(r,c) | r <- blocks !! (x `div` 3), c <- blocks !! (x `rem` 3)]) xs)

genEmptyBlockSudoku :: Int -> IO ()
genEmptyBlockSudoku n = do [r] <- rsolveNs [emptyN]
                           showNode r
                           bs <- randomize [0..8]
                           s  <- genProblem (cleanBlocks r (take n bs))
                           showNode s

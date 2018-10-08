module Lab5EmptyBlocks where

import Data.List

import Lecture5

{-
Select randomly the blocks to empty and then generate a Sudoku problem with the cleaned Sudoku.
If there is no unique solution possible this is reported.

Time spent: 1:00
-}

cleanCells :: Node -> [(Row, Column)] -> Node
cleanCells n []     = n
cleanCells n (x:xs) = eraseN (cleanCells n xs) x

cleanBlocks :: Node -> [(Row, Column)] -> Node
cleanBlocks n xs = cleanCells n (concatMap (\(r,c) -> [(r',c') | r' <- blocks !! r, c' <- blocks !! c]) xs)

checkEmptyBlockSudoku :: Int -> IO ()
checkEmptyBlockSudoku n = do [r] <- rsolveNs [emptyN]
                             showNode r
                             xs <- randomize [(r,c) | r <- [0..2], c <- [0..2]]
                             let xs' = take n xs
                             let r' = cleanBlocks r xs'
                             s  <- genProblem r'
                             if uniqueSol r' then showNode s
                             else putStrLn ("--- Failed.\n There is no unique solution possible whith clean blocks: "
                                           ++ (show (map (\(r,c) -> (r+1,c+1)) xs'))) 

module Lab5EmptyBlocks where

import Data.List

import Lecture5

{-
Select randomly the blocks to empty and then generate a Sudoku problem with the cleaned Sudoku.
If there is no unique solution possible this is reported.

3 empty blocks is possible:
checkEmptyBlockSudoku 3
+-------+-------+-------+
| 4 2 6 | 3 5 8 | 9 1 7 |
| 3 7 9 | 2 4 1 | 8 5 6 |
| 1 8 5 | 9 7 6 | 4 3 2 |
+-------+-------+-------+
| 6 3 8 | 1 9 4 | 2 7 5 |
| 5 4 1 | 6 2 7 | 3 8 9 |
| 7 9 2 | 5 8 3 | 6 4 1 |
+-------+-------+-------+
| 2 6 4 | 8 1 5 | 7 9 3 |
| 9 1 7 | 4 3 2 | 5 6 8 |
| 8 5 3 | 7 6 9 | 1 2 4 |
+-------+-------+-------+
+-------+-------+-------+
| 4 2   | 3   8 |       |
|     9 | 2   1 |       |
|     5 | 9 7   |       |
+-------+-------+-------+
| 6 3   |       |     5 |
|     1 |       |   8   |
| 7     |       |   4   |
+-------+-------+-------+
|   6   |       | 7     |
| 9     |       |     8 |
| 8 5 3 |       | 1     |
+-------+-------+-------+


4 empty blocks is possible:
checkEmptyBlockSudoku 4
+-------+-------+-------+
| 6 4 1 | 2 7 8 | 5 3 9 |
| 9 3 2 | 5 6 4 | 7 8 1 |
| 8 7 5 | 9 1 3 | 4 2 6 |
+-------+-------+-------+
| 2 9 6 | 1 3 5 | 8 7 4 |
| 4 1 8 | 6 2 7 | 9 5 3 |
| 7 5 3 | 4 8 9 | 6 1 2 |
+-------+-------+-------+
| 1 8 7 | 3 4 6 | 2 9 5 |
| 5 2 4 | 8 9 1 | 3 6 7 |
| 3 6 9 | 7 5 2 | 1 4 8 |
+-------+-------+-------+
+-------+-------+-------+
|       | 2 7   |       |
|       | 5   4 |       |
|       | 9 1   |       |
+-------+-------+-------+
| 2     |       |   7 4 |
| 4   8 |       |   5 3 |
| 7 5   |       | 6   2 |
+-------+-------+-------+
|   8   |       |   9   |
|       |       |   6 7 |
| 3 6 9 |       | 1     |
+-------+-------+-------+


Time spent: 1:00
-}

cleanCells :: Node -> [(Row, Column)] -> Node
cleanCells n []     = n
cleanCells n (x:xs) = eraseN (cleanCells n xs) x

cleanBlocks :: Node -> [(Row, Column)] -> Node
cleanBlocks n xs = cleanCells n (concatMap (\(r,c) -> [(r',c') | r' <- blocks !! (r-1), c' <- blocks !! (c-1)]) xs)

checkEmptyBlockSudoku :: Int -> IO ()
checkEmptyBlockSudoku n = do [r] <- rsolveNs [emptyN]
                             showNode r
                             xs <- randomize [(r,c) | r <- [1..3], c <- [1..3]]
                             let xs' = take n xs
                             let r' = cleanBlocks r xs'
                             if uniqueSol r' then do
                                s  <- genProblem r'
                                showNode s
                             else putStrLn ("--- Failed.\n There is no unique solution possible whith clean blocks: "
                                           ++ (show  xs')) 

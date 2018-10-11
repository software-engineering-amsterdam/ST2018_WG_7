module Lab5_Ex4

where

import Data.List

import Lecture5

{-
 Time spend: 1.5 hours 
 Efficient code that returns minimal sudokus with three empty blocks.
-}
eraseBlock :: Node -> (Row,Column) -> Node
eraseBlock n (r,c) = foldr (\rc n' -> eraseN n' rc) n [(r',c') | r' <- bl r, c' <- bl c]

-- We don't want 3 empty blocks in the same row or column because the problem would be ambiguous
notSameSubgridRowOrCol :: (Row, Column) -> (Row, Column) -> Bool
notSameSubgridRowOrCol (r,c) (r', c') = bl r /= bl r' && bl c /= bl c'

-- Erases three random blocks
erase3Blocks :: Node -> IO Node
erase3Blocks n = do ys <- randomize blocks
                    let block1 = head ys
                    let block2And3 = take 2 (filter (notSameSubgridRowOrCol block1) ys)
                    let node = foldr (\b n' -> eraseBlock n' b) n (block1:block2And3)
                    return node
                    where 
                      blocks = [(r,c) | r <- [1,4,7], c <- [1,4,7]]

-- This *might* become an infinite loop, as likely as you'd win roulette many times in a row.
runExercise4 :: IO ()
runExercise4 = do r <- genRandomSudoku
                  showNode r
                  sudokuWithThreeEmptyBlocks <- erase3Blocks r
                  if (uniqueSol sudokuWithThreeEmptyBlocks) then do
                    showNode sudokuWithThreeEmptyBlocks
                    s <- genProblem sudokuWithThreeEmptyBlocks
                    showNode s
                  else 
                    runExercise4


{-
 Code that is more flexible: makes sudokus with n empty blocks. If it returns,
 it returns a minimal sudoku, otherwise it tries again. Beware using n=5 as it
 might take very long, higher than 5 results in an infinite loop as this is
 theoretically impossible.
 A solution is generated and then n blocks are randomly emptied. If there is a unique solution
 to this Sudoku the Sudoku problem is printed. If there is nu unique solution another solution is 
 genereated, and printend, for which three blocks are cleared and the Sudoku is checked. This process
 continues until time runs out or a unique solution is found.
-}

cleanCells :: Node -> [(Row, Column)] -> Node
cleanCells n []     = n
cleanCells n (x:xs) = eraseN (cleanCells n xs) x

cleanBlocks :: Node -> [(Row, Column)] -> Node
cleanBlocks n xs = cleanCells n (concatMap (\(r,c) -> [(r',c') | r' <- blocks !! (r-1), c' <- blocks !! (c-1)]) xs)

shuffleBlocks :: IO [(Row, Column)]
shuffleBlocks = randomize [(r,c) | r <- [1..3], c <- [1..3]]

{-
The code below has been written just before the deadline and has not been tested thoroughly, hence it is
left as comment. It is meant to ensure there are at most 2 blocks in a row or column that will
be emptied.
It can easily being enabled by uncommenting it together with line 82 and placing line 81 in comment.
-}

-- no3Adjacent :: [(Row, Column)] -> Bool
-- no3Adjacent xs = all (\ys -> length ys < 3) [ filter (\(r,c) -> r == r') xs | r' <- [1..3]] &&
--                  all (\ys -> length ys < 3) [ filter (\(r,c) -> c == c') xs | c' <- [1..3]]

-- -- Randomizes the order of the blocks and ensures that there are no three empty
-- -- blocks adjacent in a row or column
-- blocksToEmpty :: Int -> IO [(Row, Column)]
-- blocksToEmpty n = do xs <- shuffleBlocks
--                      let xs' = take n xs
--                      if no3Adjacent xs' then return (sort xs')
--                      else blocksToEmpty n

checkEmptyBlockSudoku :: Int -> IO ()
checkEmptyBlockSudoku n = do [r] <- rsolveNs [emptyN]
                             showNode r
                             xs <- shuffleBlocks
                             -- xs <- blocksToEmpty n
                             let xs' = take n xs
                             let r' = cleanBlocks r xs'
                             if uniqueSol r' then do
                                s  <- genProblem r'
                                showNode s
                             else checkEmptyBlockSudoku n
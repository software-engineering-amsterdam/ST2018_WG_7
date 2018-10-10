module Exercise4

where

import Data.List

import Lecture5

{-
 Time spend: 1.5 hours 
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

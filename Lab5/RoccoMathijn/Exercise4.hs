module Exercise4

where

import Data.List

import Lecture5

{-
 Time spend: 1.5 hours 
-}
eraseBlock :: Node -> (Row,Column) -> Node
eraseBlock n (r,c) = foldr (\rc n' -> eraseN n' rc) n [(r',c') | r' <- bl r, c' <- bl c]

-- We don't want 2 empty blocks in the same row or column because then there won't be a unique solution
notSameSubgridRowOrCol :: (Row, Column) -> (Row, Column) -> Bool
notSameSubgridRowOrCol (r,c) (r', c') = bl r /= bl r' && bl c /= bl c'

-- Erases three random blocks
erase3Blocks :: Node -> IO Node
erase3Blocks n = do ys <- randomize blocks
                    let block1 = head ys
                    let ys' = filter (notSameSubgridRowOrCol block1) ys
                    let block2 = head ys'
                    let ys'' = filter (notSameSubgridRowOrCol block2) ys'
                    let block3 = head ys''
                    let node = foldr (\b n' -> eraseBlock n' b) n [block1,block2,block3]
                    return node
                    where 
                      blocks = [(r,c) | r <- [1,4,7], c <- [1,4,7]]

runExercise4 :: IO ()
runExercise4 = do r <- genRandomSudoku
                  showNode r
                  sudokuWithThreeEmptyBlocks <- erase3Blocks r
                  showNode sudokuWithThreeEmptyBlocks
                  s <- genProblem sudokuWithThreeEmptyBlocks
                  showNode s

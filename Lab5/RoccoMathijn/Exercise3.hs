module Exercise3

where

import Data.List

import Lecture5
{-
Exercise 3

A Sudoku problem P is minimal if it admits a unique solution, and every problem P' you can get from P by erasing one of the hints admits more than one solution. How can you test whether the problems generated by the code given in the lecture notes are minimal?

Deliverables: testing code, test report, indication of time spent.
-}

{-
Time spend: 2,5 hours
-}


--Finds all parents of a node
parents :: Node -> [Node]
parents (s, _) = [(parent, constraints parent) 
                  | r <- positions, 
                    c <- positions, 
                    let parent = extend s ((r,c), 0), 
                    elem (r,c) $ filledPositions s
                  ]

{-
Applying length to unknown lists is generally a bad idea, both practically due to infinite lists,
and conceptually because often it turns out that you don't actually care about the length anyway.
Slow solution:
admitsOneSolution :: Node -> Bool
admitsOneSolution node = length $ solveNs [node] == 1

Faster solution:
isNonEmpty [] = False
isNonEmpty (_:_) = True

longerThan :: Int -> [a] -> Bool
longerThan n xs = isNonEmpty $ drop n xs

admitsOneSolution :: Node -> Bool
admitsOneSolution node = not (longerThan 1 (solveNs [node]))
-- https://stackoverflow.com/questions/7371730/how-to-tell-if-a-list-is-infinite

I now found the uniqueSol function in the Lecture5 module. Using that one.
-}

minimalSudoku :: Node -> Bool
minimalSudoku node = not $ any uniqueSol (parents node)

randomMinimalSudoku = do 
                        [r] <- rsolveNs [emptyN]
                        genProblem r

-- runs in about 30 seconds                        
runExercise3 = do
                  putStrLn "-- == Exercise 3 Minimal sudokus == --"
                  putStrLn "Testing 10 sudokus ..."
                  randomSudokus <- sequence [randomMinimalSudoku | _ <- [0..10]]
                  let result = filter (not . minimalSudoku) randomSudokus
                  if null result then putStrLn "+++ OK, Tested 10 sudokus" else do
                    putStrLn "--- Failed, found a more minimal sudoku for: " 
                    showNode $ head result

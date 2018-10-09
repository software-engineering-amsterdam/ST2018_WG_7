module Lab5_Ex5

where

import Lab5_Ex1
import Data.List
import System.Random

-- Exercise 5:
-- All the code required was already present thanks to ex1:
-- the genProblem function implicitly uses the new NRC constraint, thus creating an NRC sudoku.

main5 :: IO ()
main5 = do 
          [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s


{-
 Tests for our generator:
 We assume our solver works. We can then use this solver to test our generator;
 our generator uses a finished sudoku and empties it partially. If it empties it
 incorrectly, the solver may not find the same sudoku the generated started out
 with. Running this test multiple times makes it likely our generator works as desired.
-}
equals :: Node -> Node -> Bool
equals (s1, _) (s2, _) = filledPositions s1 == filledPositions s2

test :: IO ()
test = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
          let [solved] = solveNs [s]
          showNode solved
          if (r `equals` solved) then putStrLn "+++ OK" else putStrLn "Generated and solved sudokus are not the same"
          
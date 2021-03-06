module Lab5MinimalizeTesting

where 

import Lecture5

{-
Build upon the code from exercise 2.

If the code from the lecture produces minimal problems this would mean 
that there is no unique solution when any hint is being removed.
What we do is that we check for every single hint if there are multiple
solutions once the hint is erased. If so that would mean the Sudoku is
a minimal Sudoku.

Given the code from the lecture we can have the existing code generate
a Sudoku problem and then using the technique described above to test
if it is a minimal Soduku.

Time spent: 0:40, optimizing the solution by reusing existing methods.
-}

none :: [Bool] -> Bool
none = all not

isMinimal :: Sudoku -> Bool
isMinimal s = none (map uniqueSol [eraseN n p | p <- filledPositions s])
  where n = head (initNode (sud2grid s))

minimalizeTesting :: IO ()
minimalizeTesting = do [r] <- rsolveNs [emptyN]
                       showNode r
                       s  <- genProblem r
                       showNode s
                       putStrLn ("Minimal: " ++ (show (isMinimal (fst s))))

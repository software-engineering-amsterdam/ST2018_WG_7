module Lab5 where

import Lecture5

import Data.List

-- == Exercise 1 == --
{-
The goal of this exercise is to extend the Sudoku program described in the
lecture of this week with functions that can also handle Sudokus of a special
kind: the Sudokus that appear in the Dutch evening newspaper NRC-Handelsblad 
each week (designed by Peter Ritmeester, from Oct 8, 2005 onward). These NRC 
Sudokus are special in that they have to satisfy a few extra constraints: 
in addition to the usual Sudoku constraints, each of the 3×3 subgrids with 
left-top corner (2,2), (2,6), (6,2), and (6,6) should also yield an injective 
function. The above figure gives an example (this is the NRC sudoku that appeared 
Saturday Nov 26, 2005).

Your task is to formalize this extra constraint, and to use your formalization 
in a program that can solve this Sudoku. See also the webpage of Andries Brouwer
Deliverables: modified Sudoku solver, solution to the above puzzle, indication of
time spent.
-}

exercise1 :: Grid
exercise1 = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

showRowNrcNormal :: [Value] -> IO()
showRowNrcNormal [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; 
     putChar ' '         ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showRowNrc :: [Value] -> IO()
showRowNrc [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGridNrc :: Grid -> IO()
showGridNrc [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+---------+---------+")
    showRowNrcNormal as; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowNrc bs; showRowNrc cs
    putStrLn ("+---------+---------+---------+")
    showRowNrc ds; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowNrcNormal es; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowNrc fs
    putStrLn ("+---------+---------+---------+")
    showRowNrc gs; showRowNrc hs; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowNrcNormal is
    putStrLn ("+---------+---------+---------+")

blocksNrc :: [[Int]]
blocksNrc = [[2,3,4],[6,7,8]]

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc 

subgridNrc :: Sudoku -> (Row,Column) -> [Value]
subgridNrc s (r,c) = 
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSubgridNrc :: Sudoku -> (Row,Column) -> [Value]
freeInSubgridNrc s (r,c) = freeInSeq (subgridNrc s (r,c))

freeAtPosNrc :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNrc s (r,c) = (freeInRow s r) 
                        `intersect` (freeInColumn s c) 
                        `intersect` (freeInSubgrid s (r,c)) 
                        `intersect` (freeInSubgridNrc s (r,c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where 
   vs = filter (/= 0) (subgridNrc s (r,c))

consistentNrc :: Sudoku -> Bool
consistentNrc s = consistent s && and
                    [ subgridInjectiveNrc s (r,c) | r <- [2,6], c <- [2,6]]

extendNodeNrc :: Node -> Constraint -> [Node]
extendNodeNrc (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         pruneNrc (r,c,v) constraints) | v <- vs ]

pruneNrc :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
pruneNrc _ [] = []
pruneNrc (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = 
        (x,y,zs\\[v]) : pruneNrc (r,c,v) rest            
  | otherwise = (x,y,zs) : pruneNrc (r,c,v) rest

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y

                       
initNodeNrc :: Grid -> [Node]
initNodeNrc gr = let s = grid2sud gr in 
                 if (not . consistentNrc) s then [] 
                 else [(s, constraintsNrc s)]

constraintsNrc :: Sudoku -> [Constraint] 
constraintsNrc s = sortBy length3rd 
    [(r,c, freeAtPosNrc s (r,c)) | 
                       (r,c) <- openPositions s ]

solveNsNrc :: [Node] -> [Node]
solveNsNrc = search succNodeNrc solved 

succNodeNrc :: Node -> [Node]
succNodeNrc (s,[]) = []
succNodeNrc (s,p:ps) = extendNodeNrc (s,ps) p 

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNsNrc (initNodeNrc gr)

showSudokuNrc :: Sudoku -> IO()
showSudokuNrc = showGridNrc . sud2grid

showNodeNrc :: Node -> IO()
showNodeNrc = showSudokuNrc . fst

solveShowNsNrc :: [Node] -> IO[()]
solveShowNsNrc = sequence . fmap showNodeNrc . solveNsNrc

main' = do
        putStrLn "-- == Exercise 1 == --"
        solveAndShowNrc exercise1

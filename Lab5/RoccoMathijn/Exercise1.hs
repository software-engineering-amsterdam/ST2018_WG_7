module Exercise1 where

import Data.List
import System.Random

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

{-
Time spend: 3,5 hours
-}

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

blocksNrc :: [[Int]]
blocksNrc = [[2,3,4],[6,7,8]]

showVal :: Value -> String
showVal 0 = " "
showVal d = show d

showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putStr "  "
     putStr (showVal a2) ; putStr "  "
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putStr "  "
     putStr (showVal a5) ; putChar ' '
     putChar ' '         ; putChar ' '
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putStr "  "
     putStr (showVal a8) ; putStr "  "
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showRowNrc :: [Value] -> IO()
showRowNrc [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putChar '|'         ; 
     putStr (showVal a2) ; putStr "  "
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a6) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putStr "  "
     putStr (showVal a8) ; 
     putChar '|'         ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn ("+---------+---------+---------+")
    showRow as; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowNrc bs; showRowNrc cs
    putStrLn ("+---------+---------+---------+")
    showRowNrc ds; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRow es; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRowNrc fs
    putStrLn ("+---------+---------+---------+")
    showRowNrc gs; showRowNrc hs; 
    putStrLn ("|   +-----|--+   +--|-----+   |")
    showRow is
    putStrLn ("+---------+---------+---------+")

type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

blNrc :: Int -> [Int]
blNrc x = concat $ filter (elem x) blocksNrc 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- bl r, c' <- bl c ]

subgridNrc :: Sudoku -> (Row,Column) -> [Value]
subgridNrc s (r,c) = 
  [ s (r',c') | r' <- blNrc r, c' <- blNrc c ]

freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = 
  freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = 
  freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeInNrc :: Sudoku -> (Row,Column) -> [Value]
freeInNrc s (r,c) = freeInSeq (subgridNrc s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = (freeInRow s r) 
                        `intersect` (freeInColumn s c) 
                        `intersect` (freeInSubgrid s (r,c)) 
                        `intersect` (freeInNrc s (r,c))

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (subGrid s (r,c))

subgridInjectiveNrc :: Sudoku -> (Row,Column) -> Bool
subgridInjectiveNrc s (r,c) = injective vs where 
   vs = filter (/= 0) (subgridNrc s (r,c))
                    
consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ subgridInjectiveNrc s (r,c) | 
                    r <- [2,6], c <- [2,6]]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = 
   [(extend s ((r,c),v),
     sortBy length3rd $ 
         prune (r,c,v) constraints) | v <- vs ]

prune :: (Row,Column,Value) 
      -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblockNrc (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest     
  | otherwise = (x,y,zs) : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

sameblockNrc :: (Row,Column) -> (Row,Column) -> Bool
sameblockNrc (r,c) (x,y) = blNrc r == blNrc x && blNrc c == blNrc y

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == 0 ]

length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd 
    [(r,c, freeAtPos s (r,c)) | 
                       (r,c) <- openPositions s ]

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 

grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = sequence . fmap showNode . solveNs

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

runExercise1 = do
        putStrLn "-- == Exercise 1 == --"
        solveAndShow exercise1

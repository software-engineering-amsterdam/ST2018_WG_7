module Sjoerd_Lab4 where

import Data.List
import Data.Typeable
import Data.Tuple
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd


{-
 Todo:
    1
    2 done
    3: test report
    4
    5 is done
    6 is done
    7: test report. Are my tests conclusive?
    8 is done.
-}


-------------------------------------------------------------------------------
-- == Assignment 2: QuickCheck tester for sets == --


instance (Arbitrary a, Eq a) => Arbitrary (Set a) where
    arbitrary = do 
                x <- arbitrary
                return (Set (nub (x)))
-- Time: 30 


randomListGen :: Int -> Int -> IO [Int]
randomListGen 0 maxN = return []
randomListGen n maxN = do
        p <- getStdRandom (randomR ((-maxN), maxN))
        ps <- randomListGen (n-1) maxN
        return (p:ps)

maxLen :: Int
maxLen = 30
maxN :: Int
maxN = 30

randomSetGen :: IO (Set Int)
randomSetGen = do
        size <- getStdRandom (randomR (0, maxLen))
        randomList <- randomListGen size maxN
        return (Set (nub(randomList)))
-- Time: 30min, even though it's mostly copy pasted.

-------------------------------------------------------------------------------
-- == Assignment 3: set operations == --
-- Helper code, to check whether two sets contain the same elements.
isEqualSet :: Eq a => Set a -> Set a -> Bool
isEqualSet (Set r) (Set s) = length r == length s && and [elem x s | x <- r ]

-- Operation definitions
setIntersect :: Eq a => Set a -> Set a -> Set a
setIntersect (Set r) (Set s) = Set [x | x <- r, elem x s]
-- test properties: an intersection is at most as big as the biggest of r and s.
--                  must contain any element both in r and in s
--                  my not contain any element that is not both in r and s.

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion (Set r) (Set s) = Set (nub (r++s))
-- test properties: lenght of intersection is at least as big as the biggest of r and s.
--                  must contain any element that is in r or in s
--                  may not contain any element not in r or in s.

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set r) (Set s) = Set (concat [ x | x <- (groupBy (==) (sort (r++s))), length x == 1])
-- test properties: lenght of intersection is at most the sum of sizes of r and s
--                  must contain any element that is either in r or in s
--                  may not contain any element not in r or in s, but not both.

-- Helper code for testing 
checkFuncsHelper :: (Ord a, Eq a) => Set a -> Set a -> Bool
checkFuncsHelper r s = isEqualSet (setUnion (setDifference r s) (setIntersect r s)) (setUnion r s)


-- My own tester. Runs 100 tests
myCheckFuncs :: IO ()
myCheckFuncs = do
        rs <- sequence [randomSetGen | _ <- [0..100]]
        ss <- sequence [randomSetGen | _ <- [0..100]]
        -- return [checkFuncsHelper r s | r <- rs, s <- ss]
        if and [checkFuncsHelper r s | r <- rs, s <- ss] then putStrLn "passed 100 tests" else putStrLn "failed a test"


-- QuickTest test functions.
checkFuncsInt :: Set Int -> Set Int -> Bool
checkFuncsInt r s = checkFuncsHelper r s

checkFuncsStr :: Set String -> Set String -> Bool
checkFuncsStr r s = checkFuncsHelper r s

ass3Tester = do
    putStrLn "\n-- == Assignment 3: set operations == --"
    putStrLn "Testing with my own set generator"
    myCheckFuncs
    putStrLn "Testing with quickCheck"
    quickCheck checkFuncsInt
    quickCheck checkFuncsStr

-- Time: 1h30min

-------------------------------------------------------------------------------
-- == Assignment 4: haskell questions: 2nd edition == --


-------------------------------------------------------------------------------
-- == Assignment 5: symmetric closure == --

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos rs = (nub(concat [ [r, swap(r)] | r <- rs ]))

-- Time: 5min

-------------------------------------------------------------------------------
-- == Assignment 6: transetive closure == --

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

isEqualRel :: (Ord a, Eq a) => Rel a -> Rel a -> Bool
isEqualRel r s = length r == length s && and [elem x s | x <- r ]

trClos :: Ord a => Rel a -> Rel a 
trClos rs = if isEqualRel rs ss then rs else trClos ss
    where
        ss = nub ((rs @@ rs) ++ rs)

-- Time: 20min
-------------------------------------------------------------------------------
-- == Assignment 7: testing 5 & 6 == --

symClosTestHelper :: Ord a => Rel a -> Bool
symClosTestHelper rs = and [elem (swap r) rSymClos | r <- rs]
    where rSymClos = symClos rs
-- test properties: every element and the reverse of every element in rs must be in rs
--                  but no more than that, and no duplicates
--                  the symmetric closure of rs is at least as big as rs.

-- 
trClosTestHelper :: Ord a => Rel a -> Bool 
trClosTestHelper rs = all ((flip elem) rsTrClos ) (rsTrClos@@rsTrClos)
    where rsTrClos = trClos (nub rs)
-- test properties: every element in the @@ of the transitive closure of rs with itself, must be contained in the transitive closure of rs
--                  transitive closure of rs is at least as big as rs

symClosTesterInt :: Rel Int -> Bool
symClosTesterInt rs = symClosTestHelper rs

symClosTesterStr :: Rel String -> Bool
symClosTesterStr rs = symClosTestHelper rs

trClosTesterInt :: Rel Int -> Bool
trClosTesterInt rs = trClosTestHelper rs

trClosTesterStr :: Rel String -> Bool
trClosTesterStr rs = trClosTestHelper rs


ass7Tester = do
    putStrLn "\n-- == Assignment 7: testing 5 & 6 == --"
    quickCheck symClosTesterInt
    quickCheck symClosTesterStr
    quickCheck trClosTesterInt
    quickCheck trClosTesterStr

-- Time: 20min

-------------------------------------------------------------------------------
-- == Assignment 8: checking (R_r)^+ == (R^+)_r == --

-- isEqualTrSymSymTr :: Rel Int -> Bool
isEqualTrSymSymTrHelper :: Ord a => Rel a -> Bool
isEqualTrSymSymTrHelper rs = trClos (symClos rs) == symClos (trClos rs)

isEqualTrSymSymTrInt :: Rel Int -> Bool
isEqualTrSymSymTrInt rs = isEqualTrSymSymTrHelper rs

isEqualTrSymSymTrStr :: Rel String -> Bool
isEqualTrSymSymTrStr rs = isEqualTrSymSymTrHelper rs

ass8Tester = do
    putStrLn "\n-- == Assignment 8: checking (R^-1)^+ == (R^+)^-1 == --"
    quickCheck (expectFailure . isEqualTrSymSymTrInt)
    quickCheck (expectFailure . isEqualTrSymSymTrStr)
    putStrLn "The tests fail, as such (R^-1)^+ /= (R^+)^-1"
    putStrLn "Counterexample: R=[(1,0)]: (R_r)^+ = [(1,0),(0,1),(1,1),(0,0)], (R^+)_r = [(1,0)]"

-- Time: 30min

-------------------------------------------------------------------------------
-- == Main == --
main = do
    ass3Tester
    ass7Tester
    ass8Tester
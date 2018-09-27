module Sjoerd_Lab4 where

import Data.List
import Data.Typeable
import Data.Tuple
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd


-------------------------------------------------------------------------------
-- == Assignment 2: QuickCheck tester for sets == --


instance (Arbitrary a, Eq a) => Arbitrary (Set a) where
    arbitrary = do 
                x <- arbitrary
                return (Set (nub (x)))

-- Time: 30 

-------------------------------------------------------------------------------
-- == Assignment 3: set operations == --

isEqualSet :: Eq a => Set a -> Set a -> Bool
isEqualSet (Set r) (Set s) = length r == length s && and [elem x s | x <- r ]

setIntersect :: Eq a => Set a -> Set a -> Set a
setIntersect (Set r) (Set s) = Set [x | x <- r, elem x s]

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion (Set r) (Set s) = Set (nub (r++s))

setDifference :: Ord a => Set a -> Set a -> Set a
-- setDifference (Set r) (Set s) = Set [x | x <- r, not (elem x s)]
setDifference (Set r) (Set s) = Set (concat [ x | x <- (groupBy (==) (sort (r++s))), length x == 1])

-- checkFuncs :: (Ord a, Eq a) => Set a -> Set a -> Bool
checkFuncs :: Set Int -> Set Int -> Bool
checkFuncs r s = isEqualSet (setUnion (setDifference r s) (setIntersect r s)) (setUnion r s)

checkFuncsStr :: Set String -> Set String -> Bool
checkFuncsStr r s = isEqualSet (setUnion (setDifference r s) (setIntersect r s)) (setUnion r s)

-- Time: 30min
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

trClos :: Ord a => Rel a -> Rel a 
trClos rs = if rs == ss then rs else trClos ss
    where
        ss = nub ((rs @@ rs) ++ rs)

-- Time: 20min
-------------------------------------------------------------------------------
-- == Assignment 7: testing 5 & 6 == --

symClosTestHelper :: Ord a => Rel a -> Bool
symClosTestHelper rs = and [elem (swap r) rSymClos | r <- rSymClos]
    where rSymClos = symClos rs

-- trClosTester :: (Int a, String a) => Rel a -> Bool 
trClosTestHelper :: Ord a => Rel a -> Bool 
trClosTestHelper rs = all ((flip elem) rsTrClos ) (rsTrClos@@rsTrClos)
    where rsTrClos = trClos rs

symClosTesterInt :: Rel Int -> Bool
symClosTesterInt rs = symClosTestHelper rs

symClosTesterStr :: Rel String -> Bool
symClosTesterStr rs = symClosTestHelper rs

trClosTesterInt :: Rel Int -> Bool
trClosTesterInt rs = trClosTestHelper rs

trClosTesterStr :: Rel String -> Bool
trClosTesterStr rs = trClosTestHelper rs


ass7Tester = do
    putStrLn "-- == Assignment 7: testing 5 & 6 == --"
    quickCheck symClosTesterInt
    quickCheck symClosTesterStr
    quickCheck trClosTesterInt
    quickCheck trClosTesterStr

-- Time: 20min
-------------------------------------------------------------------------------
-- == Assignment 8: checking (R^-1)^+ == (R^+)^-1 == --

-- isEqualTrSymSymTr :: Rel Int -> Bool
isEqualTrSymSymTrHelper :: Ord a => Rel a -> Bool
isEqualTrSymSymTrHelper rs = trClos (symClos rs) == symClos (trClos rs)

isEqualTrSymSymTrInt :: Rel Int -> Bool
isEqualTrSymSymTrInt rs = isEqualTrSymSymTrHelper rs

isEqualTrSymSymTrStr :: Rel String -> Bool
isEqualTrSymSymTrStr rs = isEqualTrSymSymTrHelper rs

ass8Tester = do
    putStrLn "-- == Assignment 8: checking (R^-1)^+ == (R^+)^-1 == --"
    quickCheck isEqualTrSymSymTrInt
    quickCheck isEqualTrSymSymTrStr
    putStrLn "The tests fail, as such (R^-1)^+ /= (R^+)^-1"


-------------------------------------------------------------------------------
-- == Main == --
main = do
    ass7Tester
    ass8Tester
module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck

import SetOrd


xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- ASSIGNMENT 2

-- For testing purposes we limit the range of natural numbers
maxNatural :: Int
maxNatural = 30

getRandomNatural :: IO Int
getRandomNatural = getStdRandom (randomR (1, maxNatural))

getRandomIntegers :: Int -> IO [Int]
getRandomIntegers 0 = return []
getRandomIntegers n = do
             x <- getStdRandom (randomR (-maxNatural, maxNatural))
             xs <- getRandomIntegers (n - 1) 
             return (x:xs)

integerSetGeneratorFromScratch :: IO (Set Int)
integerSetGeneratorFromScratch = do
    n <- getRandomNatural
    xs <- getRandomIntegers n
    return (Set (nub xs))

-- Time spent: 1:15

instance (Arbitrary a, Eq a) => Arbitrary (Set a) where 
    arbitrary = do
        xs <- arbitrary
        return (Set (nub xs))

-- Time spent: 1:00, trying all different kind of things and failing, until I talked to my teammates :-) 

-- Sorting is needed in order for the grouping to work. Once grouped it is a matter
-- of ensuring that the groups have a length of 1.
prop_UniqueElements :: Set Int -> Bool
prop_UniqueElements (Set xs) = all (\y -> length y == 1) (group (sort xs))

testAssignment2 = do
    putStrLn "\n--== Set Int Generator ==--"
    putStr "\nGenerator from scratch\nElements are unique: "
    xs <- integerSetGeneratorFromScratch
    putStrLn (show (prop_UniqueElements xs))
    putStrLn "\nQuickCheck Generator\nElements are unique: "
    quickCheck prop_UniqueElements

-- Time spent: 0:30

-- ASSIGNMENT 3

intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set r) (Set s) = Set (intersect r s)

unionSet' :: Eq a => Set a -> Set a -> Set a
unionSet' (Set r) (Set s) = Set (nub (union r s))

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set r) (Set s) = Set (r \\ s)

-- Common properties

prop_Intersected :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Intersected (Set r) (Set s) (Set rs) = all (\x -> (elem x r) && (elem x s)) rs &&
                                            all (\x -> (not (elem x s)) --> (not (elem x rs))) r &&
                                            all (\x -> (not (elem x r)) --> (not (elem x rs))) s

prop_Unioned :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Unioned (Set r) (Set s) (Set rs) = all (\x -> elem x rs) r && 
                                        all (\x -> elem x rs) s &&
                                        all (\x -> (elem x r) || (elem x s)) rs

prop_Differented :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Differented (Set r) (Set s) (Set rs) = all (\x -> (elem x r) `xor` (elem x s)) rs

-- QuickCheck testable propeties

prop_QuickCheckIntersected :: Set Int -> Set Int -> Bool
prop_QuickCheckIntersected r s = prop_Intersected r s rs && prop_UniqueElements rs
    where rs = intersectSet r s

prop_QuickCheckUnioned :: Set Int -> Set Int -> Bool
prop_QuickCheckUnioned r s = prop_Unioned r s rs && prop_UniqueElements rs
    where rs = unionSet' r s

prop_QuickCheckDifferented :: Set Int -> Set Int -> Bool
prop_QuickCheckDifferented r s = prop_Differented r s rs && prop_UniqueElements rs
    where rs = differenceSet r s

testAssignment3 = do
    putStrLn "\n--== Set Operations ==--"
    putStrLn "\nGenerator from scratch tests"
    putStrLn "\nQuickCheck tests"
    putStr "Intersection: "
    quickCheck prop_QuickCheckIntersected
    putStr "Union: "
    quickCheck prop_QuickCheckUnioned
    putStr "Difference: "
    quickCheck prop_QuickCheckDifferented

-- Time spent: 0:30

main = do
    testAssignment2
    testAssignment3

module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck

import SetOrd


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
unionSet' (Set r) (Set s) = Set (union r s)

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set r) (Set s) = Set (r \\ s)

-- Testing properties

extractList :: Set a -> [a]
extractList (Set r) = r

prop_Intersected :: Set Int -> Set Int -> Bool
prop_Intersected r s = all (\x -> (elem x (extractList r)) && (elem x (extractList s))) (extractList (intersectSet r s))

testAssignment3 = do
    putStrLn "\n--== Set Operations ==--"
    putStrLn "\nGenerator from scratch tests"
    putStrLn "\nQuickCheck tests"
    quickCheck prop_Intersected

-- Time spent: 0:30

main = do
    testAssignment2
    testAssignment3

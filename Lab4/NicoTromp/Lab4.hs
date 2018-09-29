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

getRandomInteger :: IO Int
getRandomInteger = getStdRandom (randomR (-maxNatural, maxNatural))

getRandomIntegers :: Int -> IO [Int]
getRandomIntegers 0 = return []
getRandomIntegers n = do
             x <- getRandomInteger
             xs <- getRandomIntegers (n - 1) 
             return (x:xs)

integerSetGeneratorFromScratch :: IO (Set Int)
integerSetGeneratorFromScratch = do
    n <- getRandomNatural
    xs <- getRandomIntegers maxNatural
    return (Set (nub xs))

-- Time spent: 1:15

instance (Arbitrary a, Eq a) => Arbitrary (Set a) where 
    arbitrary = do
        xs <- arbitrary
        return (Set (nub xs))

-- Time spent: 1:00, trying all different kind of things and failing, until I talked to my teammates :-) 

uniqueElements :: (Eq a) => [a] -> Bool
uniqueElements []     = True
uniqueElements (x:xs) = not (elem x xs) && uniqueElements xs

prop_UniqueElements :: (Eq a) => Set a -> Bool
prop_UniqueElements (Set xs) = uniqueElements xs

prop_QuickCheckUniqueElements :: Set Int -> Bool
prop_QuickCheckUniqueElements (Set xs) = all (\y -> length y == 1) (group (sort xs))

testAssignment2 = do
    putStrLn "\n--== Set Int Generator ==--"
    putStr "\nGenerator from scratch\nElements are unique: "
    xs <- integerSetGeneratorFromScratch
    putStrLn (show (prop_UniqueElements xs))
    putStrLn "\nQuickCheck Generator\nElements are unique: "
    quickCheck prop_QuickCheckUniqueElements

-- Time spent: 0:45

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
                                            all (\x -> (not (elem x r)) --> (not (elem x rs))) s &&
                                            prop_UniqueElements (Set rs)

prop_Unioned :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Unioned (Set r) (Set s) (Set rs) = all (\x -> elem x rs) r && 
                                        all (\x -> elem x rs) s &&
                                        all (\x -> (elem x r) || (elem x s)) rs &&
                                        prop_UniqueElements (Set rs)

prop_Differented :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Differented (Set r) (Set s) (Set rs) = all (\x -> (elem x r) `xor` (elem x s)) rs &&
                                            prop_UniqueElements (Set rs)

-- Build for testing own generator

testIt :: Eq a => [(Set a, Set a)] -> (Set a -> Set a -> Set a) -> (Set a -> Set a -> Set a -> Bool) -> Bool
testIt xs op p = and [ p (fst x) (snd x) (op (fst x) (snd x)) | x <- xs ]

-- QuickCheck testable propeties

prop_QuickCheckIntersected :: Set Int -> Set Int -> Bool
prop_QuickCheckIntersected r s = prop_Intersected r s rs
    where rs = intersectSet r s

prop_QuickCheckUnioned :: Set Int -> Set Int -> Bool
prop_QuickCheckUnioned r s = prop_Unioned r s rs
    where rs = unionSet' r s

prop_QuickCheckDifferented :: Set Int -> Set Int -> Bool
prop_QuickCheckDifferented r s = prop_Differented r s rs
    where rs = differenceSet r s

-- Scatch support functions

generateSetTuples :: Int -> IO [(Set Int, Set Int)]
generateSetTuples 0 = return []
generateSetTuples n = do
            r <- integerSetGeneratorFromScratch
            s <- integerSetGeneratorFromScratch
            xs <- generateSetTuples (n - 1) 
            return ((r, s):xs)

numberOfTests :: Int
numberOfTests = 100

showTest :: Int -> Bool -> String
showTest _ False = "--- Failed."
showTest n _     = "+++ OK, passed " ++ (show n) ++ " tests."

testAssignment3 = do
    putStrLn "\n--== Set Operations ==--"

    putStrLn "\nGenerator from scratch tests"
    putStr "Intersection: \t"
    xs <- generateSetTuples numberOfTests
    putStrLn (showTest numberOfTests (testIt xs intersectSet prop_Intersected))
    putStr "Union: \t\t"
    xs <- generateSetTuples numberOfTests
    putStrLn (showTest numberOfTests (testIt xs unionSet' prop_Unioned))
    putStr "Difference: \t"
    xs <- generateSetTuples numberOfTests
    putStrLn (showTest numberOfTests (testIt xs differenceSet prop_Differented))

    putStrLn "\nQuickCheck tests"
    putStr "Intersection: \t"
    quickCheck prop_QuickCheckIntersected
    putStr "Union: \t\t"
    quickCheck prop_QuickCheckUnioned
    putStr "Difference: \t"
    quickCheck prop_QuickCheckDifferented

-- Time spent: 2:30

-- ASSIGNMENT 5

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos []                  = []
symClos ((x,y):xs) | x == y = (x, x):symClos xs 
                   | x /= y = (x, y):(y, x):symClos (filter (\z -> z /= (y,x)) xs) 
-- While creating tests for the function (assignment 7) I dicovered that
-- the original implementation was not correct. It created a nuplicate of all elements.

-- Time spent: 0:30

testAssignment5 = do
    putStrLn "\n--== Symmetric Closure ==--"
    putStr "symClos test: \t"
    putStrLn (showTest 1 (symClos [(1,2),(2,3),(3,4)] == [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]))


-- ASSIGNMENT 6

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Transitive transformation
tr :: Ord a => Rel a -> Rel a
tr r = sort (nub ((r @@ r) ++ r))

-- Transitive closure
trClos :: Ord a => Rel a -> Rel a
trClos r = until (\s -> tr s == s) tr r

testAssignment6 = do
    putStrLn "\n--== Transitive Closure ==--"
    putStr "trClos test: \t"
    putStrLn (showTest 1 (trClos [(1,2),(2,3),(3,4)] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]))

-- Time spent: 1:30, mostly needed to figger out that the code in the condition
-- needed sorting and nubbing. This insight resulted in the tr function since it is 
-- used in twice.

-- ASSIGNMENT 7

-- Symmetric Closure properties

-- Self relations are defined as (x, x)
numberOfSelfRelations :: Eq a => Rel a -> Int
numberOfSelfRelations r = length (filter (\(x, y) -> x == y) r)

-- Unique relations exists but its symmetric counterpart.
numberOfSingleRelations :: Eq a => Rel a -> Int
numberOfSingleRelations r = length (filter (\(x, y) -> not (elem (y, x) r)) r)

-- The number of relations with an existing symmetric counterpart (both are counted!)
numberOfSymRelations :: Eq a => Rel a -> Int
numberOfSymRelations r = length (filter (\(x, y) -> x /= y&& elem (y, x) r) r)

-- Post condition: excluding self relations e.g. (x, x) the caridnality
-- of the symmetric closure must be even
prop_EvenCardinality :: Rel Int -> Bool
prop_EvenCardinality r = even (length r' - numberOfSelfRelations r')
    where r' = symClos (nub r)

-- Post condiftion: the cardinality of the symmetric closure must be equal to the sum of:
--   Existing symmetric relations, e.g. both (x,y) and (y,x) are present
--   Two times the number of unique relations, e.g. (x,y) exists and (y,x) is not present
--   The number of self relations, e.g. (x,x)
prop_CorrectCardinality :: Rel Int -> Bool
prop_CorrectCardinality r = numberOfSymRelations r' + 
                            2 * (numberOfSingleRelations r') + 
                            numberOfSelfRelations r' == length s
                            where s = symClos r'
                                  r' = nub r 

-- Transitive closure properties

prop_TransitiveClosure :: Rel Int -> Bool
prop_TransitiveClosure r = length (trClos r) /= 0

testAssignment7 = do
    putStrLn "\n--== Testing Symmetric and Transitive Closure ==--"

    putStrLn "\nSymmetric Closure tests"
    putStr "Even cardinality (excl. self rels): \t"
    quickCheck prop_EvenCardinality
    putStr "Correct cardinality: \t\t\t"
    quickCheck prop_CorrectCardinality

    putStrLn "\nTransitive Closure tests"
    -- quickCheck prop_TransitiveClosure

-- Time spent: 1:15

-- MAIN PROGRAM

main = do
    testAssignment2
    testAssignment3
    testAssignment5
    testAssignment6
    testAssignment7

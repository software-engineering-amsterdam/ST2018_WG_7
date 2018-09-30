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
-- While creating tests for the function (assignment 7) I discovered that
-- the original implementation was not correct. It created a duplicate of all elements.

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
-- used twice.

-- ASSIGNMENT 7

-- Symmetric Closure properties

-- Yes, quickcheck can be used.
-- 1:   The cardinality of the symmetric closure excluding any self relation, e.g. (x,x),
--      must be an even number. This is because for every relation (x,y) in the original
--      set of relatiosn the mirrored (y,x) must also be present
-- 2:   There are three different possible combination possible: a equals the number of elements
--      that match (x,x), b equals the number of elements that match the (x,y) and (y,x)
--      and c equals the number of element that match (w,z). Given any combination of these,
--      the cardinality of the symmetric closure is dedined as a + b + 2*c
-- 3:   For every relation in the original set both versions (the original (x,y)
--      and the mirrored (y,x)) must be present in the symmetric closure.
-- 4:   For every relation in the symmetric closure (x,y) either (x,y) or (y,x) must
--      be present in the original set.

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

-- Every relation in the original set must be present in its original form and in its
-- morrored form in the symmetric closure.
prop_SymmetricElementsInClosure :: Rel Int -> Bool
prop_SymmetricElementsInClosure r = all (\(x,y) -> elem (x,y) s && elem (y,x) s) r'
    where s = symClos r'
          r' = nub r

-- Every relation in the symmetric closure must be present in the same form or as its
-- morrored counterpart in the original set.
prop_ClosureElementHaveOrigin :: Rel Int -> Bool
prop_ClosureElementHaveOrigin r = all (\(x,y) -> elem (x,y) r' || elem (y,x) r') s
    where s = symClos r'
          r' = nub r

-- Transitive closure properties

-- The transitive closure of a transitive closure is equal to the original transitive closure.
prop_TransitivesDontChange :: Rel Int -> Bool
prop_TransitivesDontChange r = r' == trClos r'
    where r' = trClos (nub r)

relToList :: Ord a => Rel a -> [a]
relToList r = sort (nub (concat [ [x,y] | (x,y) <- r]))

-- All elements of the original set must be present in the transitive closure
prop_TransitiveClosurePreservesElements :: Rel Int -> Bool
prop_TransitiveClosurePreservesElements r = relToList r' == relToList (trClos r')
    where r' = nub r

-- Determines for a non transtive closure for a given start element the
-- elements that can trasitively be reached from that element.
reachables :: Ord a => a -> Rel a -> [a]
reachables x r = sort (nub (concat ([ z:(reachables z (filter (/=(y,z)) r')) | (y,z) <- r', y == x])))
    where r' = nub r

prop_TransitiveConnections :: Rel Int -> Bool
prop_TransitiveConnections r = all (\(x,_) -> reachables x r' == [ z | (y,z) <- s, x == y]) r'
    where s = trClos r'
          r' = nub r

testAssignment7 = do
    putStrLn "\n--== Testing Symmetric and Transitive Closure ==--"

    putStrLn "\nSymmetric Closure tests"
    putStr "Even cardinality (excl. self rels): \t"
    quickCheck prop_EvenCardinality
    putStr "Correct cardinality: \t\t\t"
    quickCheck prop_CorrectCardinality
    putStr "Symmetric elements exist in closure: \t"
    quickCheck prop_SymmetricElementsInClosure
    putStr "Closure element have origin: \t\t"
    quickCheck prop_ClosureElementHaveOrigin

    putStrLn "\nTransitive Closure tests"
    putStr "Transitives don't change: \t\t"
    quickCheck prop_TransitivesDontChange
    putStr "Transitive closure preserves elements: \t"
    quickCheck prop_TransitiveClosurePreservesElements
    putStr "Transitive connections: \t\t"
    quickCheck prop_TransitiveConnections

-- Time spent: 1:15


-- ASSIGNMENT 8

-- There is a difference.
-- Given the relation [(1,0)]
--
-- The transitive clusre of [(1,0)] is [(1,0)]
-- The symmetric closure of [(1,0)] is [(1,0),(0,1)] <= A
--
-- The symmetric closure of [(1,0)] is [(1,0),(0,1)]
-- The transitive closure of [(1,0),(0,1)] is [(0,0),(0,1),(1,0),(1,1)] <= B
-- 
-- Clearly the closures A and B are not equal, hence there is difference

prop_ClosuresDifference :: Rel Int -> Bool
prop_ClosuresDifference r = symClos (trClos r') == trClos (symClos r') 
    where r' = nub r

testAssignment8 = do
    putStrLn "\n--== Symmetric-Transitive Closure vs Transitive-Symmetric Closure ==--"

    putStrLn "\nSymmetric-Transitive vs Transitive-Symmetric Closure test"
    quickCheck prop_ClosuresDifference

-- The above test ussually fails on [(1,0)] or [(0,1)].

-- Time spent: 0:10

-- MAIN PROGRAM

main = do
    testAssignment2
    testAssignment3
    testAssignment5
    testAssignment6
    testAssignment7
    testAssignment8

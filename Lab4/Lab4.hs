module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck

import SetOrd


xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-------------------------------------------------------------------------------
-- == Assignment 1: haskell questions == --
{-
 No mayor questions came up, no questions came up that could not be answered amongst ourselves.
-}

-------------------------------------------------------------------------------
-- == Assignment 2: QuickCheck generator for sets == --

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

-- Quickcheck arbitrary implementation
instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = (\x -> Set (nub x)) <$> arbitrary


-- Time: 2h

-------------------------------------------------------------------------------
-- == Assignment 3: set operations and tests == --
-- Set operations:
intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set r) (Set s) = Set (intersect r s)

unionSet' :: Eq a => Set a -> Set a -> Set a
unionSet' (Set r) (Set s) = Set (nub (union r s))

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set r) (Set s) = Set (r \\ s)

--Helper code:
uniqueElements :: (Eq a) => [a] -> Bool
uniqueElements []     = True
uniqueElements (x:xs) = not (elem x xs) && uniqueElements xs

prop_UniqueElements :: (Eq a) => Set a -> Bool
prop_UniqueElements (Set xs) = uniqueElements xs

-- Common properties:

-- Must check both ways otherwise we might validate a implementation that always returns
-- the empty set.
prop_Intersected :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Intersected (Set r) (Set s) (Set rs) = all (\x -> (elem x r) && (elem x s)) rs &&
                                            all (\x -> (not (elem x s)) --> (not (elem x rs))) r &&
                                            all (\x -> (not (elem x r)) --> (not (elem x rs))) s &&
                                            prop_UniqueElements (Set rs)
-- test properties: an intersection is at most as big as the biggest of r and s.
--                  must contain any element both in r and in s
--                  may not contain any element that is not both in r and s.

prop_Unioned :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Unioned (Set r) (Set s) (Set rs) = all (\x -> elem x rs) r && 
                                        all (\x -> elem x rs) s &&
                                        all (\x -> (elem x r) || (elem x s)) rs &&
                                        prop_UniqueElements (Set rs)
-- test properties: lenght of intersection is at least as big as the biggest of r and s.
--                  must contain any element that is in r or in s
--                  may not contain any element not in r or in s.

prop_Differented :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Differented (Set r) (Set s) (Set rs) = all (\x -> not(elem x s) --> elem x rs) r &&
                                            all (\x -> not(elem x r) --> elem x rs) s &&
                                            all (\x -> (elem x r) `xor` (elem x s)) rs &&
                                            prop_UniqueElements (Set rs)
-- test properties: lenght of intersection is at most the sum of sizes of r and s
--                  must contain any element that is either in r or in s
--                  may not contain any element not in r or in s, but not both.


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

-- Scratch support functions

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


-- Our elaborate tests show that our functions work consistently, within tight constraints.
-- Time spent: 2:30

-------------------------------------------------------------------------------
-- == Assignment 4: haskell questions: 2nd edition == --
{-
 No questions arose that probably won't be answered by close study of the material.
-}

-------------------------------------------------------------------------------
-- == Assignment 5: symmetric closure == --

type Rel a  = [(a, a)]

--create a list of an element of the relation with its reverse
--add all these lists together
--remove duplicates from this cumulative list

symClos :: Ord a => Rel a -> Rel a
symClos (x:xs) = nub ([x, swap x] ++ (symClos xs))

-- Time: 10min

-------------------------------------------------------------------------------
-- == Assignment 6: transitive closure == --

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Transitive transformation
tr :: Ord a => Rel a -> Rel a
tr r = sort (nub ((r @@ r) ++ r))

-- Transitive closure
trClos :: Ord a => Rel a -> Rel a
trClos r = until (\s -> tr s == s) tr r

-- Time: 20min
-- Completion time varied heavily within our group.
-------------------------------------------------------------------------------
-- == Assignment 7: testing 5 & 6 == --



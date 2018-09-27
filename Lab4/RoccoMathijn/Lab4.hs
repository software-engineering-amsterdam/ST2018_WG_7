module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- == Exercise 2 == --
{-
Time spend: 2 hours
-}

{-
Scratch implementation

This implementation has a max length of 30 for the set and chooses random integers between -30 and 30.
-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (-n,n))

randomSet :: IO (Set Int) 
randomSet = do
              listLength <- getRandomInt 30
              list  <- sequence [getRandomInt 30 | _ <- [0..abs(listLength)]]
              return (Set (nub list))

-- Quickcheck arbitrary implementation
instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = (\x -> Set (nub x)) <$> arbitrary

-- == Exercise 3 == --
{-
Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
(Deliverables: implementations, test properties, short test report, indication of time spent.)

Time spend ~ 2 hours
-}

{-
Returns the intersection of the two sets
-}
intersection                   :: Eq a => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set [x | x <- xs, elem x ys]

{-
Returns the union of the two sets
-}
union                   :: Eq a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (nub (xs ++ ys))

{-
Returns the difference of the two sets
-}
difference                   :: Eq a => Set a -> Set a -> Set a
difference (Set xs) (Set ys) = Set [x | x <- xs, not (elem x ys)]

{-
Test property of intersection used by quickCheck and mySetCheck
-}
intersectionTest                    :: Set Int -> Set Int -> Bool
intersectionTest (Set xs) (Set ys)  = all (\x -> elem x xs && elem x ys) zs where 
                                          Set zs = intersection (Set xs) (Set ys)

{-
Test property of union used by quickCheck and mySetCheck
-}
unionTest                    :: Set Int -> Set Int -> Bool
unionTest (Set xs) (Set ys)  = all (\x -> elem x zs) xs && all (\x -> elem x zs) ys where 
                                          Set zs = Lab4.union (Set xs) (Set ys)

{-
Test property of difference used by quickCheck and mySetCheck
-}
differenceTest                    :: Set Int -> Set Int -> Bool
differenceTest (Set xs) (Set ys)  = not $ any (\x -> elem x ys) zs where 
                                          Set zs = difference (Set xs) (Set ys)
{-
Tests the set operations with my own random set generator
-}
mySetCheck :: (Set Int -> Set Int -> Bool) -> IO ()
mySetCheck f = do 
                  aSets <- sequence [randomSet | _ <- [0..100]]
                  bSets <- sequence [randomSet | _ <- [0..100]]
                  let result = all (==True) [f a b | a <- aSets, b <- bSets]
                  if (result) then print "+++ OK, passed 100 tests" else print "--- Failed" 

{-
Test runner for exercise 3
-}
testExercise3 = do 
                  putStrLn "\n--== Exercise 3 - Set operations ==--"
                  mySetCheck intersectionTest
                  quickCheck intersectionTest
                  mySetCheck unionTest
                  quickCheck unionTest
                  mySetCheck differenceTest
                  quickCheck differenceTest

-- == Exercise 5 == --
{-
Time spend: 15 minutes
-}
type Rel a = [(a,a)]
symClos :: Ord a => Rel a -> Rel a
symClos rel = nub (concat [[(a,b),(b,a)] | (a,b) <- rel])

-- == Exercise 6 == --
{-
Time spend: ~1 hour
-}
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos [x] = [x] 
trClos rel | all (\x -> elem x rel) t = nub rel
           | otherwise = trClos (t ++ rel) where
             t = rel @@ rel




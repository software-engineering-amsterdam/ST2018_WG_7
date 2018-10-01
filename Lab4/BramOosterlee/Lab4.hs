import SetOrd
import Test.QuickCheck
import Data.List
import System.Random

--Assignment 2 time 01:45

--non-quickcheck implementation

-- You can't write a functional random number generator, as a function should
-- always return the same value for given arguments. Therefore, instead of
-- return an Int, which would have to be the same Int at each call to the function,
-- we return an IO Int, which is calculated upon an internal state.
randomInt :: Int -> IO Int
randomInt n = getStdRandom (randomR (-n,n))

sizedSet :: Int -> Int -> IO (Set Int)
sizedSet size depth = do
    mySize <- ((randomInt size) >>= (\x -> return (abs x)))
    list <- sequence [(randomInt depth) | _ <- [1..mySize]]
    return (Set (nub list))

arbitrarySizedSet :: IO (Set Int)
arbitrarySizedSet = sizedSet 20 20

--quickcheck

instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = arbitrary >>= (\x -> return (Set (nub x)))

--Assignment 3 started 13:15 -
intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set []) b     = emptySet
intersectionSet (Set (x:xs)) b | (inSet x b) = (unionSet (Set [x]) (intersectionSet (Set xs) b))
                               | otherwise   = (intersectionSet (Set xs) b)

myUnionSet :: Eq a => Set a -> Set a -> Set a
myUnionSet (Set []) b = b
myUnionSet (Set a) (Set b) = Set (nub (a ++ b))

deleteSetSet :: Ord a => Set a -> Set a -> Set a
deleteSetSet (Set []) b     = b
deleteSetSet (Set (x:xs)) b = deleteSetSet (Set xs) (deleteSet x b)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference a b = deleteSetSet (intersectionSet a b) (myUnionSet a b)

-- tests
equals :: Ord a => Set a -> Set a -> Bool
equals a b = (subSet a b) && (subSet b a)

genIntersection :: (Eq a, Ord a) => Set a -> Bool
genIntersection a = equals (intersectionSet (takeSet 1 a) a) (takeSet 1 a)

testIntersection :: IO (Bool)
testIntersection = do x <- arbitrarySizedSet; return (genIntersection x)

-- I was using an equals sign here before, which didn't work because of different
-- orderings resulting in inequal Sets. I then used equals, a function which
-- checked whether a was a subset of b, and whether b was a subset of a.
-- It turns out there is a bug in the SetOrd subSet code, which causes subSet
-- To not be symmetric.
genUnion :: (Eq a, Ord a) => Set a -> Set a -> Bool
genUnion a b = equals (myUnionSet a b) (unionSet a b)

testUnion :: IO (Bool)
testUnion = do x <- arbitrarySizedSet; y <- arbitrarySizedSet; return (genUnion x y)

import SetOrd
import Test.QuickCheck
import Data.List
import System.Random

--Assignment 2 time 01:45

--non-quickcheck implementation

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

--Assignment 3
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

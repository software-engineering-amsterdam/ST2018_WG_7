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
    arbitrary = arbitrary >>= (\x -> return (Set (nub (sort x))))

--Assignment 3 time 03:00
intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set []) b     = emptySet
intersectionSet (Set (x:xs)) b | (inSet x b) = (unionSet (Set [x]) (intersectionSet (Set (sort xs)) b))
                               | otherwise   = (intersectionSet (Set xs) b)

myUnionSet :: Eq a => Set a -> Set a -> Set a
myUnionSet (Set []) b = b
myUnionSet (Set a) (Set b) = Set (nub (sort (a ++ b)))

deleteSetSet :: Ord a => Set a -> Set a -> Set a
deleteSetSet (Set []) b     = b
deleteSetSet (Set (x:xs)) b = deleteSetSet (Set (sort xs)) (deleteSet x b)

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

--todo: order sets upon creation
genUnion :: (Eq a, Ord a) => Set a -> Set a -> Bool
genUnion a b = equals (myUnionSet a b) (unionSet a b)

testUnion :: IO (Bool)
testUnion = do x <- arbitrarySizedSet; y <- arbitrarySizedSet; return (genUnion x y)

--Assignment 5, time 00:20
type Rel a  = [(a, a)]

--create a list of an element of the relation with its reverse
--add all these lists together
--remove duplicates from this cumulative list

symClos :: Ord a => Rel a -> Rel a
symClos (x:xs) = nub ([x, (snd x, fst x)] ++ (symClos xs))

--Assignment 6, time 00:11
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
	nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rel 	| (nub rel) == (nub ((nub rel) @@ (nub rel)))	= rel
			| otherwise										= trClos (nub ((nub rel) ++ ((nub rel) @@ (nub rel))))

--Assignment 8, time 00:06
--Yes, there is a difference. The difference can be found when considering the identity relation, [(x,x)].

--Let a be the relation [(1,2)] is an example which shows there is a difference between trClos . symClos and symClos . trClos.
--The symmetric closure of a is [(1,2), (2,1)].
--The transitive closure of the symmetric closure of a is [(1,2), (2,1), (1,1), (2,2)].

--Lets take a look at when we flip the function composition.
--The transitive closure of a is [(1,2)].
--The symmetric closure of the transitive closure of a is [(1,2), (2,1)]

--Here we see that the symmetric closure of the transitive closure, does not always equal the transitive closure of the symmetric closure.


module Sjoerd_Lab2 where
 
import Data.List
import Data.Char
import Data.Typeable
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall = flip all

-----------------------------------------------------------------------------------
-- Exercise Curry

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
        p <- getStdRandom random
        ps <- probs (n-1) 
        return (p:ps)

sortShit :: [Float] -> [[Float]]
sortShit ioList = groupBy (\x y ->  floor (4 * x) == floor (4 * y) ) (sort ioList)

mapSort :: Int -> IO [[Float]]
mapSort n = fmap sortShit (probs n)

probsLengths :: Int -> IO[Int]
probsLengths n = fmap (map length) (mapSort n)

-- time: 45 min
-- Thanks to Nico and Rocoo for introducing me to the existance of groupBy and fmap.
-----------------------------------------------------------------------------------
-- Exercise Triangles
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Int -> Int -> Int -> Shape
triangle x y z | a + b <= c && a > 0 = NoTriangle
               | a == c = Equilateral
               | a == b || b == c = Isosceles
               | a^2 + b^2 == c^2 = Rectangular
               | otherwise = Other
               where 
                a = sort[x, y, z] !! 0
                b = sort[x, y, z] !! 1
                c = sort[x, y, z] !! 2

-- time: 10 min

testNoTriangle 

--Credits to Nico for the idea of sorting the input for triangle.
-----------------------------------------------------------------------------------
-- Exercise Strenght tester
threeOne :: Int -> Bool
threeOne x = even x && x > 3

threeTwo :: Int -> Bool
threeTwo x = even x || x > 3

threeThree :: Int -> Bool
threeThree x = (even x && x > 3) || even x

threeFour :: Int -> Bool
threeFour x = even x

myPredicates :: [(String, (Int -> Bool))]
myPredicates = [("threeOne", threeOne), ("threeTwo", threeTwo), ("threeThree", threeThree)
                , ("threeFour", threeFour)]


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

strictlyStronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyStronger xs p q = stronger xs p q && not (weaker xs p q)

strictlyWeaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyWeaker xs p q = not (stronger xs p q) && weaker xs p q

equallyStrong :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
equallyStrong xs p q = stronger xs p q && weaker xs p q

strengthChecker xs p q | strictlyStronger xs p q = LT
                       | equallyStrong xs p q = EQ
                       | strictlyWeaker xs p q = GT


-- Sorts predicates strongest to weakest
sortPredicates :: [a] -> [(String, (a -> Bool))] -> [(String, (a -> Bool))]
sortPredicates xs predicates = sortBy (\p q -> strengthChecker xs (snd p) (snd q) ) predicates

mySortedPredicates :: [(String, (Int -> Bool))]
mySortedPredicates = sortPredicates [(-10)..10] myPredicates

interactPredicates :: [(String, (a -> Bool))] -> a -> [Bool]
interactPredicates predicates a = [ snd p a | p <- predicates]

interactMySortedPredicates :: Int -> [Bool]
interactMySortedPredicates a = interactPredicates mySortedPredicates a

sorterTest :: Int -> Bool
sorterTest a = sortedBools == sort (sortedBools) 
                where sortedBools = interactMySortedPredicates a

mySortedPredicateStrings :: [String]
mySortedPredicateStrings = [fst p | p <- mySortedPredicates]

-- I'm very proud of this. The idea to test the ordering of the result in this way was mine
-- Credits to Rens for thinking of displaying the predicate names.
-- time: 60 min

-----------------------------------------------------------------------------------

-- Exercise Recognizing Permutations

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation list1 list2 = elem list1 (permutations list2)

isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 list1 list2 = length list1 == length list2 && and [elem x list2 | x <- list1]

--isPermutationTest = 
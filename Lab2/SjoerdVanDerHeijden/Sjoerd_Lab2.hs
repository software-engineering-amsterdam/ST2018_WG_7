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

std :: [Int] -> Float
std xs = sqrt (fromIntegral (sum ([(x - avg)^2 | x <- xs ]) `div` (length xs)))
        where
         avg = (sum xs `div` length xs)

printLengthsAndStd :: Int -> IO()
printLengthsAndStd n = do
                        lengths <- probsLengths n
                        putStrLn (show lengths)
                        putStrLn (show (std lengths))

-- time: 45 min
-- Thanks to Nico and Rocco for introducing me to the existance of groupBy and fmap.
-----------------------------------------------------------------------------------
-- Exercise Triangles
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Int -> Int -> Int -> Shape
triangle x y z | a + b <= c || a < 1 = NoTriangle
               | a == c = Equilateral
               | a == b || b == c = Isosceles
               | a^2 + b^2 == c^2 = Rectangular
               | otherwise = Other
               where 
                a = sort[x, y, z] !! 0
                b = sort[x, y, z] !! 1
                c = sort[x, y, z] !! 2

-- Credits to Nico for the idea of sorting the input for triangle.
-- time: 10 min

testNoTriangle :: Int -> Int -> Bool
testNoTriangle a b = triangle a b (a+b+1) == NoTriangle

testEquilateral :: (Positive Int) -> Bool
testEquilateral (Positive a) = triangle a a a == Equilateral

testIsosceles :: (Positive Int) -> (Positive Int) -> Bool
testIsosceles (Positive a) (Positive b) | a == b = triangle a (b+1) (b+1) == Isosceles
                                        | b <= a `div` 2 = triangle b a a == Isosceles
                                        | otherwise = triangle a b b == Isosceles

testRectangular :: (Positive Int) -> Bool
testRectangular (Positive a) = triangle (3*a) (4*a) (5*a) == Rectangular

-- Thanks to Nico again
testRectangular2 :: Bool
testRectangular2 = and [triangle a b c == Rectangular | a <-[0..100], c <- [a+1..200], b <- [a+1..c-1], a^2 + b^2 == c^2] 


-- testOther (Positive a) (Positive b) | a+b <= 4 = triangle (a+1) (b+3) (a+b+3) == Other
--                                     |a == b = triangle (b+1) (b+3) (b+4) == Other
--                                     | otherwise = triangle a b (head [c| c <- [maximum([a,b])+1..a+b-1], triangle a b c /= Rectangular, a+b > c]) == Other

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




-----------------------------------------------------------------------------------
main = do
        putStrLn "Random number distribution and standard deviation:"
        printLengthsAndStd 20000
        putStrLn "Testing NoTriangle"
        quickCheck testNoTriangle
        putStrLn "Testing Equilateral"
        quickCheck testEquilateral
        putStrLn "Testing Isosceles"
        quickCheck testIsosceles
        putStrLn "Testing Rectangular"
        quickCheck testRectangular
        putStrLn "Testing Rectangular2"
        quickCheck testRectangular2

        putStrLn "Printing the ordering from strongest to weakest predicates"
        print mySortedPredicateStrings
        putStrLn "Testing the predicate sorter"
        quickCheck sorterTest

module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- ASSIGNMENT 1 - DISTRIBUTION --

-- Map every quartile to a integer number starting from 0
-- (0..0.25)   -> 0
-- [0.25..0.5) -> 1
-- [0.5..0.75) -> 2
-- [0.75..1)   -> 3
floatToClass :: Float -> Integer
floatToClass x = toInteger (floor (4.0 * x))

-- Check if two values belong to the same 
isSameClass :: Float -> Float -> Bool
isSameClass x y = floatToClass x == floatToClass y

-- Group the sorted values according to their class
groupValues :: [Float] -> [[Float]]
groupValues xs = groupBy isSameClass (sort xs)

-- Generates a number of random values, divides them into quartiles, determines
-- the distribution and print the results, including the maximum deviation from
-- the mean. The mean is defined as the number of generated numbers over 4.
-- The number of random numbers that needs to be generated is the input parameter
-- of this function.
checkRandomness :: Int -> IO ()
checkRandomness n = do
                      values <- probs n
                      let quartiles = groupValues values
                      let distribution = map length quartiles
                      putStr "Number of values in each quartile: "
                      print distribution
                      let mean = n `div` 4
                      let maxDeviation = maximum (map (\x -> abs(x - mean)) distribution)
                      putStr "The maximum deviation from the mean: "
                      print maxDeviation
                      putStr "That is as percentage: "
                      print ((100 * maxDeviation) `div` mean)

distributionTest = do
  putStrLn "\n--== DISTRIBUTION ==--\n"
  checkRandomness 10000

-- Time spend: 1 hour


-- ASSIGNMENT 2 - TRIANGLES --

-- The following properties hold if and only if the parameters are provided in
-- ascending order! The calling function is responsible for ensuring this.

-- The input is illegal if there are any values less then or equal to zero
isIllegal :: Int -> Int -> Int -> Bool
isIllegal a b c = a <= 0

-- All sides are equal
isEquilateral :: Int -> Int -> Int -> Bool
isEquilateral a b c = a == b && b == c

-- Either it is a flat or a sharp isosceles triangle.
isIsosceles :: Int -> Int -> Int -> Bool
isIsosceles a b c = (a == b && a + b > c) || b == c 

-- Pythagorean check
isRectangular :: Int -> Int -> Int -> Bool
isRectangular a b c = a^2 + b^2 == c^2

-- When ordered ascending the sum of a and b must be larger then c
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c

-- The properties are placed in descending strength from top to bottom.
-- If a property does not hold a weaker property is tested.
-- All property check expect the parameters are sorted in ascending order.
triangle :: Int -> Int -> Int -> Shape
triangle x y z | isIllegal a b c     = NoTriangle
               | isEquilateral a b c = Equilateral
               | isIsosceles a b c   = Isosceles
               | isRectangular a b c = Rectangular
               | isTriangle a b c    = Other
               | otherwise           = NoTriangle
               where
                  abc = sort [x, y, z]
                  a = abc !! 0
                  b = abc !! 1
                  c = abc !! 2
-- Time spend: 2:00 including a large amount of refactoring and bug fixing

-- Shufles the elements of the array by selecting one of the permutations
-- by using the number n
rearrange :: Int -> [a] -> [a]
rearrange n xs = xss !! (n `mod` (length xss))
               where xss = permutations xs

-- Any number less zero can't result in a triangle.
-- By making one positive number negative this condition is assured.
-- By shuffeling the order of the values we make sure that the negative number
-- is randomy distributed over the sides.
negativeLengthTest :: (Positive Int) -> (Positive Int) -> (Positive Int) -> (Positive Int) -> Bool
negativeLengthTest (Positive x) (Positive y) (Positive z) (Positive i) = triangle a b c == NoTriangle
                          where
                            abc = rearrange i [-x, y, z]
                            a = abc !! 0
                            b = abc !! 1
                            c = abc !! 2

-- By shuffeling the order of the values (with a single zero) we make sure that the zero
-- is randomy distributed over the sides.
zeroLengthTest :: (Positive Int) -> (Positive Int) -> (Positive Int) -> Bool
zeroLengthTest (Positive x) (Positive y) (Positive i)= triangle a b c == NoTriangle
                          where
                            abc = rearrange i [x, y, 0]
                            a = abc !! 0
                            b = abc !! 1
                            c = abc !! 2

-- The sides of a equilateral triangles are all the same, so we make one by using a single
-- random number generated by QuickCheck
equilateralTest :: (Positive Int) -> Bool
equilateralTest (Positive x) = triangle x x x == Equilateral

-- When testing isosceles triangles there are two different situations. 
--   1: The isosceles sides are bigger then the non-isosceles side
--   2: The isosceles sides are less then the non-isosceles side and sum of the
--      isosceles sides is greater then the length of the non-isosceles side.
-- If QuickCheck generates a even number we generate lengths that match the first situation
-- otherwise we generate the second situation. The same parameter is also used to randomly
-- select a permutation for the order of the sides.
-- By adding 2 to the maximum of the two we ensure that it is really larger then the minimum.
isoscelesTest :: (Positive Int) -> (Positive Int) -> (Positive Int) -> Bool
isoscelesTest (Positive x) (Positive y) (Positive i) = triangle a b c == Isosceles
                          where
                            maxxy = (max x y) + 2
                            minxy = min x y
                            abc | even i = rearrange i [maxxy, maxxy, minxy]
                                | odd i  = rearrange i [1 + (maxxy `div` 2), 1 + (maxxy `div` 2), maxxy]
                            a = abc !! 0
                            b = abc !! 1
                            c = abc !! 2

-- Test for non-triangles by adding 1 to the sum of the two numbers sides.
noTriangleTest :: (Positive Int) -> (Positive Int) -> (Positive Int)-> Bool
noTriangleTest (Positive x) (Positive y) (Positive i) = triangle a b c == NoTriangle
                          where
                            abc = rearrange i [x, y, x + y + 1]
                            a = abc !! 0
                            b = abc !! 1
                            c = abc !! 2

-- Test all rectangular triangles.
-- The values for the sides are generated using the m-n formula from:
-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
rectangularTest :: (Positive Int) -> (Positive Int) -> (Positive Int) -> Bool
rectangularTest (Positive x) (Positive y) (Positive i) = triangle a b c == Rectangular
                          where
                            m = 1 + max x y
                            n = min x y
                            abc = rearrange i [m^2 - n^2, 2 * m * n, m^2 + n^2]
                            a = abc !! 0
                            b = abc !! 1
                            c = abc !! 2

-- Properties that should not be true for a triangle to be a 'regular' triangle.
notAllowedProperties :: [(Int -> Int -> Int -> Bool)]
notAllowedProperties = [isIllegal, isEquilateral, isIsosceles, isRectangular]

-- Unfortunately this test auses most of code the triangle function uses.
regularTest :: (Positive Int) -> (Positive Int) -> (Positive Int) -> (Positive Int) -> Bool
regularTest (Positive x) (Positive y) (Positive z) (Positive i) = not (or [ p a b c | p <- notAllowedProperties ]) && isTriangle a b c --> True
                    where
                      abc = rearrange i [x, y, z]
                      a = abc !! 0
                      b = abc !! 1
                      c = abc !! 2

triangleTests = do
  putStrLn "\n--== TRIANGLE ==--\n"
  putStrLn "Negative length side test."
  quickCheck negativeLengthTest 
  putStrLn "Zero length size test."
  quickCheck zeroLengthTest 
  putStrLn "Not a triangle test."
  quickCheck noTriangleTest 
  putStrLn "Equilateral triangle test."
  quickCheck equilateralTest 
  putStrLn "Isosceles triangle test."
  quickCheck isoscelesTest 
  putStrLn "Rectangular triangle test."
  quickCheck rectangularTest
  putStrLn "Regular triangle test."
  quickCheck regularTest

-- Time spend: 4:00


-- ASSIGNMENT 3 - PROPERTY STRENGTH --

instance Show (a->b) where
  show f = "<function>"

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

strictlyStronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyStronger xs p q = stronger xs p q && not (weaker xs p q)

stricktlyWeaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stricktlyWeaker xs p q = weaker xs p q && not (stronger xs q p)

equivelant :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
equivelant xs p q = stronger xs p q && weaker xs p q

type PropertyInfo = (String, (Int -> Bool))

comparator :: PropertyInfo -> PropertyInfo -> Ordering
comparator f g | strictlyStronger xs f' g' = LT
               | stronger xs f' g'         = LT
               | equivelant xs f' g'       = EQ
               | weaker xs f' g'           = GT
               | stricktlyWeaker xs f' g'  = GT
               where
                xs = [(-10)..10]
                f' = snd f
                g' = snd g

properties :: [PropertyInfo]
properties = [("even", even), 
  ("(\\ x -> even x && x > 3)", (\ x -> even x && x > 3)),
  ("(\\ x -> even x || x > 3)", (\ x -> even x || x > 3)),
  ("(\\ x -> (even x && x > 3) || even x)", (\ x -> (even x && x > 3) || even x))]

orderedProperties :: [PropertyInfo]
orderedProperties = sortBy comparator properties

orderedPropertyTexts :: [String]
orderedPropertyTexts = [ fst p | p <- orderedProperties]

inputDomain :: [Int]
inputDomain = [(-10)..10]

orderedPropertyResults :: [String]
orderedPropertyResults = [ show ( filter (snd p) inputDomain) | p <- orderedProperties]

printStrings :: [String] -> IO()
printStrings[]      = putStrLn ""
printStrings (x:xs) = do {
      putStrLn x;
      printStrings xs;
    }

printOrdereProperties = do
  putStrLn "\n--== PROPERTY STRENGHT ==--\n"
  putStrLn "Properties in descending order of strength."
  printStrings orderedPropertyTexts
  putStrLn "And their corresponding results (given [-10..10] as the domain for the input)."
  printStrings orderedPropertyResults

-- Time spend: 2:00

-- This function runs all the tests
main = do
  distributionTest
  triangleTests
  printOrdereProperties

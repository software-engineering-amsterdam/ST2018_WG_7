
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


-- ASSIGNMENT 1 --

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
                      putStrLn "Number of values in each quartile."
                      print distribution
                      let mean = n `div` 4
                      let maxDeviation = maximum (map (\x -> abs(x - mean)) distribution)
                      putStrLn "The maximum deviation from the mean "
                      print maxDeviation
                      putStrLn "That is as percentage"
                      print ((100 * maxDeviation) `div` mean)

-- Time spend: 1 hour


-- ASSIGNMENT 2 --

-- The input is illegal if there are any values less then or equal to zero
isIllegal :: Int -> Int -> Int -> Bool
isIllegal a b c = a <= 0

-- Isosceles all values must be equal
isIsosceles :: Int -> Int -> Int -> Bool
isIsosceles a b c = a == b && b == c

-- After sorting the values accending, the first two values be equal and it
-- must be a triangle 
isEquilateral :: Int -> Int -> Int -> Bool
isEquilateral a b c = b == c

-- After sorting the values accending, the first two values be greater then
-- the largest
isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c

-- After sorting the values accending they can be used using the Pythagorean 
-- equation
isRectangular :: Int -> Int -> Int -> Bool
isRectangular a b c = a^2 + b^2 == c^2

-- The checks are placed in order of their priority from top to bottom.
-- If a check with higher priority is not satisfied, checks with lower priority
-- are executed. All checks asume that the parameters are sorted in ascending order.
triangle :: Int -> Int -> Int -> Shape
triangle x y z | isIllegal a b c     = NoTriangle
               | isIsosceles a b c   = Isosceles
               | isEquilateral a b c = Equilateral
               | isRectangular a b c = Rectangular
               | isTriangle a b c    = Other
               | otherwise           = NoTriangle
               where
                  abc = sort [x, y, z]
                  a = abc !! 0
                  b = abc !! 1
                  c = abc !! 2
-- Time spend: 1:45

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


-- ASSIGNMENT 2 --

-- The input is illegal if there are any values less then or equal to zero
isIllegal :: Int -> Int -> Int -> Bool
isIllegal a b c = length (filter (<=0) [a, b, c]) > 0

-- Isosceles all values must be equal
isIsosceles :: Int -> Int -> Int -> Bool
isIsosceles a b c = a == b && b == c

-- After sorting the values accending, the first two values be equal and it
-- must be a triangle 
isEquilateral :: [Int] -> Bool
isEquilateral xs = (a == b) && isTriangle xs
                 where a = head ys
                       b = head (tail ys)
                       ys = sort xs

-- After sorting the values accending, the first two values be greater then
-- the largest
isTriangle :: [Int] -> Bool
isTriangle xs = a + b > c
              where a = head ys
                    b = head (tail ys)
                    c = head (tail (tail ys)) 
                    ys = sort xs

-- After sorting the values accending they can be used using the Pythagorean 
-- equation
isRectangular :: [Int] -> Bool
isRectangular xs = a^2 + b^2 == c^2
              where a = head ys
                    b = head (tail ys)
                    c = head (tail (tail ys)) 
                    ys = sort xs

-- The checks are placed in order of their priority from top to bottom.
-- If a check with higher priority is not satisfied, checks with lower priority
-- are executed.
triangle :: Int -> Int -> Int -> Shape
triangle a b c | isIllegal a b c         = NoTriangle
               | isIsosceles a b c       = Isosceles
               | isEquilateral [a, b, c] = Equilateral
               | isRectangular [a, b, c] = Rectangular
               | isTriangle [a, b, c]    = Other
               | otherwise               = NoTriangle

-- Time spend: 1:30
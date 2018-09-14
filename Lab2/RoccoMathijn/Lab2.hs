module Workshop2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
(-->) :: Bool -> Bool -> Bool

p --> q = (not p) || q

data Shape =  NoTriangle | Equilateral| Isosceles  | Rectangular | Other 
              deriving (Eq,Show)

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

-- 1 -- ~ 1 hour
quartileTest :: IO ()
quartileTest =  do
                    randomFloats <- probs 10000
                    let grouped = groupBy (\x y -> floor(x * 4) == floor(y * 4)) (sort randomFloats)
                    putStrLn (show (map length grouped))

-- 2 -- 30 minutes on triangle and 4 hours on test
triangle        :: Integer -> Integer -> Integer -> Shape
triangle x y z  | a + b < c || a < 0          = NoTriangle
                | a == b && a == c            = Equilateral
                | a == b || a == c || b == c  = Isosceles
                | a * a + b * b == c * c      = Rectangular
                | otherwise                   = Other
                where sorted = sort [x, y, z]
                      a = sorted !! 0
                      b = sorted !! 1
                      c = sorted !! 2

data Triangle = Triangle Shape (Integer, Integer, Integer)
                deriving (Eq,Show)

instance Arbitrary Triangle where
    arbitrary = do  index <- choose(0, 4)
                    let shape = [NoTriangle, Equilateral, Isosceles, Rectangular, Other] !! index
                    sides <- generateTriangle shape
                    return $ Triangle shape sides

pythagoreanTriplets :: [(Integer, Integer, Integer)]
pythagoreanTriplets = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..c], a^2 + b^2 == c^2, a < b]

otherTriangles :: [(Integer, Integer, Integer)]
otherTriangles = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..c], a^2 + b^2 /= c^2, a < b, b < c, c < (a + b)]

isoscelesTriangles :: [(Integer, Integer, Integer)]
isoscelesTriangles = [(a, a, b) | b <- [1..], a <- [1..b], a /= b, 2*a > b]

generateTriangle              :: Shape -> Gen (Integer, Integer, Integer)
generateTriangle NoTriangle   = do  a <- arbitrary
                                    b <- arbitrary
                                    return (a, b, a + b + 1)
generateTriangle Equilateral  = do  Positive n <- arbitrary
                                    return (n, n, n)
generateTriangle Isosceles    = do  Positive n <- arbitrary
                                    return (isoscelesTriangles !! n)
generateTriangle Rectangular  = do  Positive n <- arbitrary
                                    return (pythagoreanTriplets !! n)
generateTriangle Other        = do  Positive n <- arbitrary
                                    return (otherTriangles !! n)

triangleTest :: Triangle -> Bool
triangleTest (Triangle shape (x, y, z)) = shape == triangle x y z

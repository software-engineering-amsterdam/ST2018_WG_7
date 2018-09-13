
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

--Assignment 1, started at

--Assignment 2, started at 11:15
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | (any (\x -> x == True) [side <= 0 | side <- [a, b, c]]) = NoTriangle
                | (a>b+c) || (b>a+c) || (c>a+b)                           = NoTriangle
                | (a<b-c) || (b<a-c) || (c<a-b)                           = NoTriangle
                | (a<c-b) || (b<c-a) || (c<b-a)                           = NoTriangle
                | (a == b) && (a==c)                                      = Equilateral
                | a*a+b*b==c*c                                            = Rectangular
                | (a==b) || (a==c) || (b==c)                              = Isosceles
                | otherwise                                               = Other

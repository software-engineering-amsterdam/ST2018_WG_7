
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

--Assignment 2, time 00:25
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | (any (\x -> x == True) [side <= 0 | side <- [a, b, c]]) = NoTriangle
                | (a>b+c) || (b>a+c) || (c>a+b)                           = NoTriangle
                | (a<b-c) || (b<a-c) || (c<a-b)                           = NoTriangle
                | (a<c-b) || (b<c-a) || (c<b-a)                           = NoTriangle
                | (a == b) && (a==c)                                      = Equilateral
                | a*a+b*b==c*c                                            = Rectangular
                | (a==b) || (a==c) || (b==c)                              = Isosceles
                | otherwise                                               = Other

--Assignment 3, time 02:15
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

--a.
prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 x = even x
prop2 x = (even x && x > 3)
prop3 x = (even x || x > 3)
prop4 x = ((even x && x > 3) || even x)

--b.
strongerOrdering xs p q | (stronger xs p q) && (stronger xs q p) = EQ
                        | stronger xs p q = LT
                        | stronger xs q p = GT

orderedproptuples = sortBy (\x y -> strongerOrdering [-10..10] (fst x) (fst y)) [(prop1, "prop1"), (prop2, "prop2"), (prop3, "prop3"), (prop4, "prop4")]
orderedpropnames = [snd x | x <- orderedproptuples]

--Assignment 4, started 14:20-15:05
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b | length a /= length b = False
                  | otherwise            = all (==True) [elem x b | x <- a]

-- test1 = isPermutation [] []
test2 = isPermutation [] [1, 2, 3]
test3 = isPermutation [1, 2, 3] [1, 2, 3]
test4 = isPermutation [1, 2, 3] [3, 2, 1]
-- test5 = isPermutation [[]] [[]]
-- test6 = isPermutation [[]] [[[]]]
test7 = isPermutation [[[]]] [[[1, 2, 3]]]
test8 = isPermutation [[1, 2, 3]] [[1, 2, 3]]
test9 = isPermutation [[1, 2, 3]] [[3, 2, 1]]

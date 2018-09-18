
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--Assignment 1, time taken 01:00
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

sameCategory :: Float -> Float -> Bool
sameCategory x y = floor (4*x) == floor (4*y)

buckets :: [Int]
buckets = [length l | l <- (groupBy sameCategory (probs 10000))]

expectedLength :: Int
expectedLength = 10000 `div` 4

deviates :: Float -> Bool
deviates d = any [abs (a-expectedLength) >= d | a <- buckets]

deviates 100

--Assignment 2, time 00:25
data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

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

--Assignment 4, time taken 00:45
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

--Assignment 6, time taken 00:30

--Specification of ROT13
--ROT13 is a specific caesar cipher, meaning that each character is substituted
--by another character with one chosen offset in the alphabet.
--With ROT13, we use the offset 13, which makes it it's own inverse,
--because (x+13)+13 = x+26, and (x+26) mod 26 = x mod 26

--First we split the input into segments of alphabetical characters and others.
--We map each character to it's position in the alphabet.
--We transform each position by adding 13.
--We then map each position back to the 26 group so we stay within the alphabet.
--Finally, we convert each position to a character again.
--We join the input to it's non alphabetical characters again.

rot13 :: [Char] -> [Char]
rot13 phrase = [rot13ifapplicable char | char <- phrase]

lowercasecheck :: Char -> Bool
lowercasecheck char = char >= 'a' && char <= 'z'

uppercasecheck :: Char -> Bool
uppercasecheck char = char >= 'A' && char <= 'Z'

lowercasetoindex :: Char -> Int
lowercasetoindex char = ord char - ord 'a'

uppercasetoindex :: Char -> Int
uppercasetoindex char = ord char - ord 'A'

indextolowercase :: Int -> Char
indextolowercase index = chr (ord 'a' + index)

indextouppercase :: Int -> Char
indextouppercase index = chr (ord 'A' + index)

lowercaserot13 :: Char -> Char
lowercaserot13 char = indextolowercase (rot13char (lowercasetoindex char))

uppercaserot13 :: Char -> Char
uppercaserot13 char = indextouppercase (rot13char (uppercasetoindex char))

rot13ifapplicable :: Char -> Char
rot13ifapplicable char | lowercasecheck char = lowercaserot13 char
                       | uppercasecheck char = uppercaserot13 char
                       | otherwise           = char

rot13char :: Int -> Int
rot13char char = (char + 13) `mod` 26
























--

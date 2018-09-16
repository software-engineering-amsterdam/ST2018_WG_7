module Workshop2 where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck
import qualified Data.Map as Map


infix 1 --> 
(-->) :: Bool -> Bool -> Bool

p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

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

-- 3 -- 15 minutes

stronger        :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker          :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker   xs p q = stronger xs q p 

evenAndGT3 :: Int -> Bool
evenAndGT3 = (\ x -> even x && x > 3)

evenOrGT3 :: Int -> Bool
evenOrGT3 = (\ x -> even x || x > 3)

evenAndGT3OrEven :: Int -> Bool
evenAndGT3OrEven = (\ x -> (even x && x > 3) || even x)

-- TODO Provide a descending strength list of all the implemented properties.

-- 4 -- 10 minutes
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = forall xs (\x -> elem x ys) && forall ys (\y -> elem y xs)

-- Next, define some testable properties for this function, and use a number of well-chosen lists to test isPermutation. You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?

-- Provide an ordered list of properties by strength using the weakear and stronger definitions.

-- Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.

-- Deliverables: Haskell program, concise test report, indication of time spent.

-- 5 -- 10 minutes
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] ys = False
isDerangement xs [] = False
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys 

deran :: Eq a => [a] -> [[a]]
deran xs = [p | p <- permutations xs, isDerangement xs p]

-- 6 -- 5 minutes
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
                    | otherwise = c
lowers :: String -> Int
lowers xs = length [x | x <- xs, x>= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x | x' <- xs , x' == x]

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- TODO 
-- First, give a specification of ROT13.

-- Next, give a simple implementation of ROT13.

-- Finally, turn the specification into a series of QuickCheck testable properties, and use these to test your implementation.

-- -- 7 ~ 1 hour
iban :: String -> Bool
iban x = checkIbanLength x && computeRemainder(interpretAsInteger (replaceLettersWithDigits (reArrangeIban x))) == 1

checkIbanLength :: String -> Bool
checkIbanLength x = ibanLength == countryLength where
                      ibanLength = length x
                      countryLength = fromMaybe 0 (Map.lookup (take 2 x) countryLengths)

reArrangeIban :: String -> String
reArrangeIban x = drop 4 x ++ take 4 x

replaceLettersWithDigits :: String -> String
replaceLettersWithDigits x = concat (map replaceLetterWithDigit x)

replaceLetterWithDigit :: Char -> String
replaceLetterWithDigit x = if (isAlpha x) then show (ord (toUpper x) - 55) else show (ord x - 48)

interpretAsInteger :: String -> Integer
interpretAsInteger x = read x

computeRemainder :: Integer -> Integer
computeRemainder x = x `mod` 97

countryLengths = Map.fromList [("AL", 28), ("AD", 24), ("AT", 20), ("AZ", 28), ("BH", 22), ("BY", 28), ("BE", 16), ("BA", 20), ("BR", 29), ("BG", 22), ("CR", 22), ("HR", 21), ("CY", 28), ("CZ", 24), ("DK", 18), ("DO", 28), ("SV", 28), ("EE", 20), ("FO", 18), ("FI", 18), ("FR", 27), ("GE", 22), ("DE", 22), ("GI", 23), ("GR", 27), ("GL", 18), ("GT", 28), ("HU", 28), ("IS", 26), ("IQ", 23), ("IE", 22), ("IL", 23), ("IT", 27), ("JO", 30), ("KZ", 20), ("XK", 20), ("KW", 30), ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), ("MK", 19), ("MT", 31), ("MR", 27), ("MU", 30), ("MD", 24), ("MC", 27), ("ME", 22), ("NL", 18), ("NO", 15), ("PK", 24), ("PS", 29), ("PL", 28), ("PT", 25), ("QA", 29), ("RO", 24), ("LC", 32), ("SM", 27), ("ST", 25), ("SA", 24), ("RS", 22), ("SC", 31), ("SK", 24), ("SI", 19), ("ES", 24), ("SE", 24), ("CH", 21), ("TL", 23), ("TN", 24), ("TR", 26), ("UA", 29), ("AE", 23), ("GB", 22), ("VG", 24)]

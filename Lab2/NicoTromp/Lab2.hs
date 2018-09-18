
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

-- Check if two values belong to the class 
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

-- Time spent: 1 hour


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
-- Time spent: 2:00 including a large amount of refactoring and bug fixing

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

-- Time spent: 4:00


-- ASSIGNMENT 3 - PROPERTY STRENGTH --

instance Show (a->b) where
  show f = "<function>"

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

equivelant :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
equivelant xs p q = stronger xs p q && weaker xs p q

-- Holds a printable and a executable version of a property.
type PropertyInfo = (String, (Int -> Bool))

inputDomain :: [Int]
inputDomain = [(-10)..10]

-- Comparator for sorting the properties n descending strength
comparator :: PropertyInfo -> PropertyInfo -> Ordering
comparator f g | stronger inputDomain f' g'         = LT
               | equivelant inputDomain f' g'       = EQ
               | weaker inputDomain f' g'           = GT
               where
                f' = snd f
                g' = snd g

-- Properties to sort
properties :: [PropertyInfo]
properties = [("even", even), 
  ("(\\ x -> even x && x > 3)", (\ x -> even x && x > 3)),
  ("(\\ x -> even x || x > 3)", (\ x -> even x || x > 3)),
  ("(\\ x -> (even x && x > 3) || even x)", (\ x -> (even x && x > 3) || even x))]

orderedProperties :: [PropertyInfo]
orderedProperties = sortBy comparator properties

orderedPropertyTexts :: [String]
orderedPropertyTexts = [ fst p | p <- orderedProperties]

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
  putStrLn "As can seen above there are two properties that yield the same result (given the input domain)."
  putStrLn "This means that tese two properties have a equivelant strength. As a result the order of their"
  putStrLn "declaration and the exact implementation of the sorting algorithm determines which one is placed"
  putStrLn "before the other and not their strengths."

-- Time spent: 2:00


-- ASSIGNMENT 4 - PERMUTATIONS --

elementsNotAtSameLocation :: Eq a => [a] -> [a] -> Bool
elementsNotAtSameLocation [] []         = True
elementsNotAtSameLocation (x:xs) (y:ys) = x /= y && elementsNotAtSameLocation xs ys

haveSameElements :: Eq a => [a] -> [a] -> Bool
haveSameElements [] []     = True
haveSameElements [] _      = False
haveSameElements (x:xs) ys = haveSameElements xs (delete x ys)

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && elementsNotAtSameLocation xs ys && haveSameElements xs ys

-- equal lengths
-- [i] != [i]
-- [i] = [j], i != j

-- Time spent: 0:45

isPermutationTests = do
    putStrLn "\n--== PERMUTATIONS ==--\n"
    putStrLn "In the following tests the expected outcome is printed between parentheses.\n"
    putStrLn "Lengths must be equal."
    putStr "\"Haskell\" \"QuickCheck\" have different lengths, isPermutation? (false) : "
    putStrLn (show (isPermutation "Haskell" "QuickCheck"))
    putStrLn "Can't be equal."
    putStr "\"QuickCheck\" \"QuickCheck\" are equal, isPermutation? (false) : "
    putStrLn (show (isPermutation "QuickCheck" "QuickCheck"))
    putStrLn "Elements must match."
    putStr "\"QuickCheck\" \"uickCheckQ\" are rotated, isPermutation? (true) : "
    putStrLn (show (isPermutation "QuickCheck" "uickCheckQ"))
    putStrLn "Elements must match."
    putStr "\"QuickCheck\" \"Zygomorphy\" don't share any character, isPermutation? (false) : "
    putStrLn (show (isPermutation "QuickCheck" "Zygomorphy"))

-- Time spent: 1:00

-- ASSIGNMETN 5 - DERANGEMENT (SINTERKLAAS) --

checkDerangement :: Int -> [Int] -> Bool
checkDerangement _ [] = True
checkDerangement n (x:xs) = n /= x && checkDerangement (n+1) xs

isDerangement :: [Int] -> Bool
isDerangement xs = checkDerangement 0 xs && isPermutation xs ys && xs /= ys
                 where
                    ys = [0..((length xs)-1)]

-- [i] != i
-- isPermutation of [0..(n-1)]

-- Time spent: 0:15

deran :: Int -> [[Int]]
deran n = [ xs | xs <- permutations [0..(n-1)], isDerangement xs ]


-- ASSIGNMENT 6 - ROT 13 --

-- Map uppercase to uppercase and lowercase to lowercase.
-- First map character to integer (a => 0), add 13 and keep within 26,
-- then map back to character
rot13 :: Char -> Char
rot13 c | c >= 'a' && c <= 'z' = chr ((((ord c - ord 'a') + 13) `mod` 26) + ord 'a')
        | c >= 'A' && c <= 'Z' = chr ((((ord c - ord 'A') + 13) `mod` 26) + ord 'A')
        | otherwise            = c

rotate13 :: String -> String
rotate13 xs = [ rot13 x | x <- xs ]

-- Time spent: 0:20


-- ASSIGNMENT 7 - IBAN --

isCorrectLength :: String -> Bool
isCorrectLength xs = length xs > 4

isAlphaNumeric :: String -> Bool
isAlphaNumeric xs = all isAlphaNum xs

isKnownISOCode :: String -> Bool
isKnownISOCode _ = True

startsWithISOCode :: String -> Bool
startsWithISOCode xs = all (\x -> isAlpha x && isUpper x) (take 2 xs) && isKnownISOCode (take 2 xs)

hasCheckDigits :: String -> Bool
hasCheckDigits xs = all isNumber (drop 2 (take 4 xs))

rearrangeIBAN :: String -> String
rearrangeIBAN xs = drop 4 xs ++ take 4 xs

translateChars :: String -> String
translateChars []     = []
translateChars (x:xs) | isAlpha x = show (ord x - ord 'A' + 10) ++ (translateChars xs)
                      | otherwise = x:translateChars xs

isValidIBAN :: String -> Bool
isValidIBAN xs = (read (translateChars (rearrangeIBAN xs)) :: Integer) `mod` 97 == 1

iban :: String -> Bool
iban xs = isCorrectLength xs && 
          isAlphaNumeric xs && 
          startsWithISOCode xs && 
          hasCheckDigits xs && 
          isValidIBAN xs

-- Time spend: 1:00

-- This function runs all the tests
main = do
  distributionTest
  triangleTests
  printOrdereProperties
  isPermutationTests

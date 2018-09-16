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

-- Divides a list with floats between 0 and 1 into four bins.
bin :: [Float] -> [[Float]]
bin ioList = groupBy (\x y ->  floor (4 * x) == floor (4 * y) ) (sort ioList)

-- Calls the probs function to create an IO list of random numbers, then maps the binning function onto it.
mapBin :: Int -> IO [[Float]]
mapBin n = fmap bin (probs n)

-- Returns the length 
probsLengths :: Int -> IO[Int]
probsLengths n = fmap (map length) (mapBin n)

-- Calculates the standard deviation
std :: [Int] -> Float
std xs = sqrt (fromIntegral (sum ([(x - avg)^2 | x <- xs ]) `div` (length xs)))
        where
         avg = (sum xs `div` length xs)

-- Prints the distribution of n random numbers over four bins, and the standard deviation of this distribution
printLengthsAndStd :: Int -> IO()
printLengthsAndStd n = do
                        lengths <- probsLengths n
                        putStrLn (show lengths)
                        putStrLn (show (std lengths))

-- time: 45 min
-- Thanks to Nico and Rocco for introducing me to the existance of groupBy and fmap.

{- The probs function generates n random numbers between 1 and 0. mapBin applies the bin 
function to these numbers which results in a list of four bins with a width of 0.25, which
 should each contain an equal amount of the random numbers. std calculates the standard deviation
 of a list, which printLengthsAndStd prints alongside the list of bins, to aid in the understanding
 of how random the randomly generated numbers really are. Inspection has shown that they are fairly random.
-}
-----------------------------------------------------------------------------------
-- Exercise Triangles
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- You know what this does. It does so by first sorting the input, then checking what kind of triangle it is.
-- Since the edges are sorted by length, fewer checks are necessary to find the type of a 
-- triangle. E.g.: instead of needing a==b,b==c to check whether a triangle is Equilateral,
-- it is only necessary to check whether a==c.
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

-- Tests for the triangle function:
-- Creates number combinations that could never represent triangles and checks whether the triangle function identifies them as such.
testNoTriangle :: Int -> Int -> Bool
testNoTriangle a b = triangle a b (a+b+1) == NoTriangle

-- Creates equilateral triangles and checks whether the triangle function identifies them as such.
testEquilateral :: (Positive Int) -> Bool
testEquilateral (Positive a) = triangle a a a == Equilateral

-- Creates isosceles (but not equilateral) triangles and checks whether the triangle function identifies them as such.
testIsosceles :: (Positive Int) -> (Positive Int) -> Bool
testIsosceles (Positive a) (Positive b) | a == b = triangle a (b+1) (b+1) == Isosceles
                                        | b <= a `div` 2 = triangle b a a == Isosceles
                                        | otherwise = triangle a b b == Isosceles

-- Creates rectengular triangles and checks whether the triangle function identifies them as such.
-- Since 3^2 + 4^2 = 5^2, (3*a)^2 + (4*a)^2 = (5*a)^2 for any a, as such always generating rectangular triangles.
-- Unfortunately, this test generates only a few of the possible rectangular triangles.
testRectangular :: (Positive Int) -> Bool
testRectangular (Positive a) = triangle (3*a) (4*a) (5*a) == Rectangular

-- Thanks to Nico again, a more exhaustive rectangular triangle test, though the range may be considered limited.
testRectangular2 :: Bool
testRectangular2 = and [triangle a b c == Rectangular | a <-[0..100], c <- [a+1..200], b <- [a+1..c-1], a^2 + b^2 == c^2] 

-- Creates triangles that are not equilateral, isosceles, or rectangular, and checks whether the triangle function identifies them as such.
-- I add a value to a and b, since triangles with a side of length 1 are never of type Other.
-- Since I sort a and b before adding 1 and 2 respectively, the resulting triangle is never equilateral.
-- Also because of that and since a+b+2 > a+1 and a+b+2 > b+2 the triangle is never isosceles.
-- The triangle is also never rectangular thanks to carefully chosen parameters (and luck and trial and error).
testOther :: (Positive Int) -> (Positive Int) -> Bool
testOther (Positive x) (Positive y) = triangle (a+1) (b+2) (a+b+2) == Other
                                        where 
                                         a = minimum[x,y]
                                         b = maximum[x,y]
                                    -- | otherwise = triangle (a+1) (b+2) (a+b+2) == Other
                                    -- | a == b = triangle (b+1) (b+3) (b+4) == Other
                                    -- | otherwise = triangle a b (head [c| c <- [maximum([a,b])+1..a+b-1], triangle a b c /= Rectangular, a+b > c]) == Other

{-
 Credits to Nico for the idea of sorting the input for triangle.
 I test and verified the triangle function explicitly for every kind of triangle (and noTriangle).
 Since all triangles are tested for, it is not necessary to implement dedicated falsification tests;
 each test is a falsification test for the others.
 total time: 60 min
-}
-----------------------------------------------------------------------------------
-- Exercise Strenght tester

property1 :: Int -> Bool
property1 x = even x && x > 3

property2 :: Int -> Bool
property2 x = even x || x > 3

-- Note: due to the || , this property is equal to simply "even x" .
property3 :: Int -> Bool
property3 x = (even x && x > 3) || even x

property4 :: Int -> Bool
property4 x = even x

-- I add strings to my properties as to be able to print the ordered list of properties later.
myProperties :: [(String, (Int -> Bool))]
myProperties = [("property1", property1), ("property2", property2), ("property3", property3)
                , ("property4", property4)]


stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

strictlyStronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyStronger xs p q = stronger xs p q && not (weaker xs p q)

strictlyWeaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyWeaker xs p q = not (stronger xs p q) && weaker xs p q

equallyStrong :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
equallyStrong xs p q = stronger xs p q && weaker xs p q

-- Returns something of type Ordering, for use with sortBy.
strengthChecker xs p q | strictlyStronger xs p q = LT
                       | equallyStrong xs p q = EQ
                       | strictlyWeaker xs p q = GT


-- Sorts properties strongest to weakest.
sortProperties :: [a] -> [(String, (a -> Bool))] -> [(String, (a -> Bool))]
sortProperties xs properties = sortBy (\p q -> strengthChecker xs (snd p) (snd q) ) properties

-- Allows me to check whether my properties apply to a given input. Returns a list of booleans: one for each 
-- property I check against it.
interactProperties :: [(String, (a -> Bool))] -> a -> [Bool]
interactProperties properties a = [ snd p a | p <- properties]

-- Checks whether my properties have been properly sorted, by checking whether the boolean
-- list returned by interactProperties is already sorted. This is a conclusive test, since
-- the output should always be sorted according to the definition of "strength" of properties.
sorterTest :: Int -> Bool
sorterTest a = sortedBools == sort (sortedBools) 
                where sortedBools = interactMySortedProperties a

-- Applies sortProperties to my own previously defined properties
mySortedProperties :: [(String, (Int -> Bool))]
mySortedProperties = sortProperties [(-10)..10] myProperties

-- Credits to Rens
-- Returns the strings of my sorted properties, to visually inspect the results of the sortProperties function
mySortedPropertiesStrings :: [String]
mySortedPropertiesStrings = [fst p | p <- mySortedProperties]

-- Applies interactMySortedProperties to my own previously defined properties
interactMySortedProperties :: Int -> [Bool]
interactMySortedProperties a = interactProperties mySortedProperties a


{-
 I take the list of properties, then use the supplied functions stronger and weaker to 
 determine their relative strength, which I express in an Ordering type. The built in 
 sortBy function then sorts them.

 I'm very proud of this. The idea to test the ordering of the result in this way was mine
 Credits to Rens for thinking of displaying the predicate names.
 time: 60 min
 -}

-----------------------------------------------------------------------------------
-- Exercise Recognizing Permutations

 -- The first thing I thought of, but slow for long lists.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation list1 list2 = elem list1 (permutations list2)

-- A faster implementation of isPermutation, but only works for lists with unique elements.
isPermutation2 :: Eq a => [a] -> [a] -> Bool
isPermutation2 list1 list2 = length list1 == length list2 && and [elem x list2 | x <- list1]

testPermutation :: Bool
testPermutation = and [
                isPermutation2 [] ([] :: [Int]), -- I had to add Int to prevent ambiguous type errors.
                isPermutation2 [1,2,3] [3,2,1],
                isPermutation2 [[]] ([[]] :: [[Int]]),
                not (isPermutation2 [1,2,3] [1,2,4]),
                not (isPermutation2 [1,2] [1,2,3]),
                isPermutation2 "abc" "cba"
                ]

quickCheckPermutations :: Eq a => [a] -> Bool
quickCheckPermutations list1 = isPermutation2 list1 (head (permutations list1))
falsifyPermutations :: Eq a => [a] -> a -> Bool
falsifyPermutations list1 x = not (isPermutation2 list1 (list1 ++ [x]))

{-
 Testable properties for isPermutation: takes any type of Eq. Takes two lists and returns
 True when they are of the same length and each element of one list is also in the other
 (assuming neither contains duplicate elements).
 I assume that a list is also a permutation of itself, I am unsure of the definition in this aspect.
 would automated testing even make sense here? If isPermutation works for [1,2,3], could it ever not work for any list?
 Automated tested is made more difficult by the fact that I assume unique elements, but quickCheck doesnt know that.
 For some mystical reason, falsifyPermutations does not work when I call it in main, but it does work from the command line.
 time: 30m
-}

-----------------------------------------------------------------------------------
-- Exercise Recognizing Derangement

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement list1 list2 = isPermutation2 list1 list2 && and [ list1 !! i /= list2 !! i | i <- [0..length(list1)-1]]

testDerangement :: Bool
testDerangement = and [
                isDerangement [] ([] :: [Int]),
                not (isDerangement [1,2,3] [3,2,1]),
                isDerangement [1,2,3] [2,3,1],
                not (isDerangement [[]] ([[]] :: [[Int]])),
                not (isDerangement [1,2,3] [1,2,4]),
                not (isDerangement [1,2] [1,2,3]),
                not (isDerangement "abc" "cba"),
                isDerangement "abc" "bca"
                ]

quickCheckDerangement list1 = isDerangement list1 (drop 1 list1 ++ [head list1])

{-
 The same constraints hold as for isPermutation, but added is the requirement that none of the original
 elements may be on the same spot as they were before.
 time: 15min
-}
-----------------------------------------------------------------------------------
-- Exercise Rot 13
{-
 Rot 13 takes a string, then replaces every letter with one 13 places away from
 it in the alphabet (e.g., a -> n, n -> a). 
-}
rot13Char :: Char -> Char
rot13Char char | char >= 'A' && char <= 'M' = chr (ord char +13)::Char
               | char >= 'N' && char <= 'Z' = chr (ord char -13)::Char
               | char >= 'a' && char <= 'm' = chr (ord char +13)::Char
               | char >= 'n' && char <= 'z' = chr (ord char -13)::Char
               | otherwise = char

rot13String :: [Char] -> [Char]
rot13String str = [rot13Char s | s <- str]

testRot13 :: [Char] -> Bool
testRot13 str = str == rot13String (rot13String str)

{-
 Since the alphabet is 13 letters long, rot13String (rot13String str) should return str
 time: 15min
-}
-----------------------------------------------------------------------------------
-- Exercise IBAN

-- Moves the first four characters of a string to the back. 
moveCharToBack :: [Char] -> [Char]
moveCharToBack string = drop 4 string ++ take 4 string

-- Turns a character to an Int according to 'A' = 10, 'B' = 11, etcetera. Does not change numbers (nor lowercase letters)
ibanCharToInt :: Char -> [Char]
ibanCharToInt char | char >= 'A' && char <= 'Z' = show (ord char -55)
                   | otherwise = [char]

-- Combines moveCharToBack and ibanCharToInt to create a list of numbers in string format, in the order required to verify the initially given IBAN number
ibanToStrList :: [Char] -> [[Char]]
ibanToStrList string = [ibanCharToInt char | char <- moveCharToBack string]

-- Takes an IBAN number and returns the integer required to validate it.
ibanToInt :: [Char] -> Integer
ibanToInt string = read [myChar | myStr <- ibanToStrList string, myChar <- myStr]::Integer

ibanValidate :: [Char] -> Bool
ibanValidate string = mod (ibanToInt string) 97 == 1

-- A list of legal IBAN numbers
testListIban :: [[Char]]
testListIban = ["AL35202111090000000001234567",
                "AD1400080001001234567890",
                "AT483200000012345864",
                "AZ96AZEJ00000000001234567890",
                "BH02CITI00001077181611",
                "BY86AKBB10100000002966000000"]

-- A list of illegal IBAN numbers
falsifyListIban :: [[Char]]
falsifyListIban = ["AL35202111090000000001234568",
                   "AD1400080001001234567891",
                   "AT483200500012345864",
                   "AZ96ASEJ00000000001234567890",
                   "BH12CITI00001077181611",
                   "BY86AKBP10100000002966000000"]

testIban = all ibanValidate testListIban
falsifyIban = not (any ibanValidate falsifyListIban)

{-
 I'm unsure about building an automated tester, as it would seem to me that in order to generate test cases, 
 it needs to do the same checks as I do in order to validate the numbers.
 time: 30min
-}

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

        putStrLn "Printing the ordering from strongest to weakest properties"
        print mySortedPropertiesStrings
        putStrLn "Testing the property sorter"
        quickCheck sorterTest

        putStrLn "Testing permutation checker"
        quickCheck testPermutation
        -- quickCheck falsifyPermutations

        putStrLn "Testing derangement checker"
        quickCheck testDerangement

        putStrLn "Testing rot13"
        quickCheck testRot13

        putStrLn "Testing IBAN verifier"
        quickCheck testIban
        quickCheck falsifyIban
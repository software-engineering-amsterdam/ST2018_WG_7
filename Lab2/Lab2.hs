module Lab2 where

import Data.List
import Data.Char
import Data.Typeable
import Test.QuickCheck
import Data.Maybe
import System.Random
import Text.Show.Functions
import qualified Data.Map as Map


infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

forany :: [a] -> (a -> Bool) -> Bool
forany = flip any

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
        p <- getStdRandom random
        ps <- probs (n-1) 
        return (p:ps)


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-----------------------------------------------------------------------------------
-- Exercise 1: Random generator checker

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
  checkRandomness 10000

-- Time spent: 1 hour
{- Aside from just binning our random values, we calculate their deviation, to aid in the understanding
 of how random the randomly generated numbers really are. Inspection has shown that they are fairly random.
-}

-----------------------------------------------------------------------------------
-- Exercise Triangles

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Since we sort our input, we require fewer checks to determine the triangle type.
triangle        :: Integer -> Integer -> Integer -> Shape
triangle x y z  | a + b < c || a < 0    = NoTriangle
                | a == c                = Equilateral
                | a == b || b == c      = Isosceles
                | a^2 + b^2 == c^2      = Rectangular
                | otherwise             = Other
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

-- The values for the sides are generated using the m-n formula from:
-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
pythagoreanTriplets :: Integer -> Integer -> (Integer, Integer, Integer)
pythagoreanTriplets x y = (m^2 - n^2, 2 * m * n, m^2 + n^2) 
                        where
                            m = 1 + max x y
                            n = min x y


otherTriangles :: [(Integer, Integer, Integer)]
otherTriangles = [(a, b, c) | 
                  c         <- [1..], 
                  b         <- [1..c-1], 
                  a         <- [1..b-1], 
                  a^2 + b^2 /= c^2, 
                  c < (a + b)]

isoscelesTriangles :: [(Integer, Integer, Integer)]
isoscelesTriangles = [(a, a, b) | b <- [1..], a <- [1..b], a /= b, 2*a > b]

-- Returns triangle generators for use by quickCheck
generateTriangle              :: Shape -> Gen (Integer, Integer, Integer)
generateTriangle NoTriangle   = do  a <- arbitrary
                                    b <- arbitrary
                                    return (a, b, a + b + 1)
generateTriangle Equilateral  = do  Positive n <- arbitrary
                                    return (n, n, n)
generateTriangle Isosceles    = do  Positive n <- arbitrary
                                    return (isoscelesTriangles !! n)
generateTriangle Rectangular  = do  Positive x <- arbitrary
                                    Positive y <- arbitrary
                                    return (pythagoreanTriplets x y)
generateTriangle Other        = do  Positive n <- arbitrary
                                    return (otherTriangles !! n)

triangleTest :: Triangle -> Bool
triangleTest (Triangle shape (x, y, z)) = shape == triangle x y z
{- Time spent: 30 minutes on triangle and 4 hours on test
 Our tests generate random triangles of a given type, then check whether our triangle function identifies them as such.
 Since all triangles are tested for, it is not necessary to implement dedicated falsification tests;
 each test is a falsification test for the others.
-}

-----------------------------------------------------------------------------------
-- ASSIGNMENT 3 - PROPERTY STRENGTH --

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
myProperties = [("even x && x > 3", property1), ("even x || x > 3", property2), ("(even x && x > 3) || even x", property3)
                , ("even x", property4)]


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
-- ASSIGNMENT 4 - PERMUTATIONS --

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && forall xs (\x -> elem x ys) && forall ys (\y -> elem y xs)
-- Time: 10 minutes

-- Manual tests
manualPermutationTests = do
    putStrLn "In the following tests the expected outcome is printed between parentheses."
    putStrLn "Lengths must be equal."
    putStr "\"Haskell\" \"QuickCheck\" have different lengths, isPermutation? (false) : "
    putStrLn (show (isPermutation "Haskell" "QuickCheck"))
    putStrLn "Can be equal."
    putStr "\"QuickCheck\" \"QuickCheck\" are equal, isPermutation? (true) : "
    putStrLn (show (isPermutation "QuickCheck" "QuickCheck"))
    putStrLn "Elements must match."
    putStr "\"QuickCheck\" \"uickCheckQ\" are rotated, isPermutation? (true) : "
    putStrLn (show (isPermutation "QuickCheck" "uickCheckQ"))
    putStrLn "Elements must match."
    putStr "\"QuickCheck\" \"Zygomorphy\" don't share any character, isPermutation? (false) : "
    putStrLn (show (isPermutation "QuickCheck" "Zygomorphy"))

-- Automated tests
isPermutationTest :: Int -> Int -> Int -> Bool
isPermutationTest a b c = forall (permutations list) (isPermutation list) where
                           list = [a, b, c]

falsifyPermutations :: [Int] -> Int -> Bool
falsifyPermutations list1 x = not (isPermutation list1 (list1 ++ [x]))

{-
 Testable properties for isPermutation: takes any type of Eq. Takes two lists and returns
 True when they are of the same length and each element of one list is also in the other
 (assuming neither contains duplicate elements).
 I assume that a list is also a permutation of itself, I am unsure of the definition in this aspect.
 Would automated testing even make sense here? If isPermutation works for [1,2,3], could it ever not work for any list?
 Automated tested is made more difficult by the fact that I assume unique elements, but quickCheck doesnt know that.
 To circumvent many issues, we hacked a test into existence, at the cost of test coverage.
 Time: 1h
-}

-----------------------------------------------------------------------------------
-- ASSIGNMENT 5 - DERANGEMENT (SINTERKLAAS) --

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement [] ys = False
isDerangement xs [] = False
isDerangement (x:xs) (y:ys) = x /= y && isDerangement xs ys 

deran :: Eq a => [a] -> [[a]]
deran xs = [p | p <- permutations xs, isDerangement xs p]

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

quickCheckDerangement :: [Int] -> Bool
quickCheckDerangement list = length uniqueList > 1 --> isDerangement uniqueList (drop 1 uniqueList ++ [head uniqueList])
        where
            uniqueList = rmdups list

{-
 The same constraints hold as for isPermutation, but added is the requirement that none of the original
 elements may be on the same spot as they were before.
 Automated tested is still made more difficult by the fact that we assume unique elements, but here we decided to explicitly dump duplicates beforehand.
 Now we run the risk of getting many lists that are too short, resulting in fewer tests being actually ran, but that's a lot better than the test coverage of 0 we had before.
 time: 15min
-}

-----------------------------------------------------------------------------------
-- ASSIGNMENT 6 - ROT 13 --
{-
Specification of ROT13
ROT13 is a specific caesar cipher, meaning that each character is substituted
by another character with one chosen offset in the alphabet.
With ROT13, we use the offset 13, which makes it it's own inverse,
because (x+13)+13 = x+26, and (x+26) mod 26 = x mod 26

First we split the input into segments of alphabetical characters and others.
We map each character to it's position in the alphabet.
We transform each position by adding 13.
We then map each position back to the 26 group so we stay within the alphabet.
Finally, we convert each position to a character again.
We join the input to it's non alphabetical characters again.
-}

rot13Char :: Char -> Char
rot13Char c | c >= 'a' && c <= 'z' = chr ((((ord c - ord 'a') + 13) `mod` 26) + ord 'a')
            | c >= 'A' && c <= 'Z' = chr ((((ord c - ord 'A') + 13) `mod` 26) + ord 'A')
            | otherwise            = c

{- The following code may seem somewhat cleaner, by using isLower and isUpper, but isLower
 and isUpper may interpret strings as unicode characters, e.g. "\42797" becomes "i".
 Unicode is gross, so we use the plain implementation of rot13Char as seen above.
-}
-- rot13Char :: Char -> Char
-- rot13Char c | isLower c = chr ((((ord c - ord 'a') + 13) `mod` 26) + ord 'a')
--             | isUpper c = chr ((((ord c - ord 'A') + 13) `mod` 26) + ord 'A')
--             | otherwise = c

rot13String :: String -> String
rot13String xs = [ rot13Char x | x <- xs ]

testRot13 :: String -> Bool
testRot13 str = str == rot13String (rot13String str)

{-
 Since the alphabet is 13 letters long, rot13String (rot13String str) should return str.
 time: 30m
-}

-----------------------------------------------------------------------------------
-- ASSIGNMENT 7 - IBAN --

iban :: String -> Bool
iban x = checkIbanLength x && (computeRemainder $ read $ replaceLettersWithDigits $ rearrangeIban x) == 1

checkIbanLength :: String -> Bool
checkIbanLength x = ibanLength == countryLength where
                      ibanLength = length x
                      countryLength = fromMaybe 0 (Map.lookup (take 2 x) countryLengths)

rearrangeIban :: String -> String
rearrangeIban x = drop 4 x ++ take 4 x

replaceLettersWithDigits :: String -> String
replaceLettersWithDigits x = concat (map replaceLetterWithDigit x)

replaceLetterWithDigit :: Char -> String
replaceLetterWithDigit x | isAlpha x = show (ord (toUpper x) - 55)
                         | otherwise = show (ord x - 48)

computeRemainder :: Integer -> Integer
computeRemainder x = x `mod` 97

countryLengths = Map.fromList [("AL", 28), ("AD", 24), ("AT", 20), ("AZ", 28), ("BH", 22), ("BY", 28), ("BE", 16), ("BA", 20), ("BR", 29), ("BG", 22), ("CR", 22), ("HR", 21), ("CY", 28), ("CZ", 24), ("DK", 18), ("DO", 28), ("SV", 28), ("EE", 20), ("FO", 18), ("FI", 18), ("FR", 27), ("GE", 22), ("DE", 22), ("GI", 23), ("GR", 27), ("GL", 18), ("GT", 28), ("HU", 28), ("IS", 26), ("IQ", 23), ("IE", 22), ("IL", 23), ("IT", 27), ("JO", 30), ("KZ", 20), ("XK", 20), ("KW", 30), ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), ("MK", 19), ("MT", 31), ("MR", 27), ("MU", 30), ("MD", 24), ("MC", 27), ("ME", 22), ("NL", 18), ("NO", 15), ("PK", 24), ("PS", 29), ("PL", 28), ("PT", 25), ("QA", 29), ("RO", 24), ("LC", 32), ("SM", 27), ("ST", 25), ("SA", 24), ("RS", 22), ("SC", 31), ("SK", 24), ("SI", 19), ("ES", 24), ("SE", 24), ("CH", 21), ("TL", 23), ("TN", 24), ("TR", 26), ("UA", 29), ("AE", 23), ("GB", 22), ("VG", 24)]

testValidIbans :: Bool
testValidIbans = forall validIbans iban

testTooLongIbans :: Bool
testTooLongIbans = not (forany tooLongIbans iban)

testWrongRemainderIban = not (forany wrongRemainderIban iban)
validIbans = ["AL35202111090000000001234567",
  "AD1400080001001234567890",
  "AT483200000012345864",
  "AZ96AZEJ00000000001234567890",
  "BH02CITI00001077181611",
  "BY86AKBB10100000002966000000",
  "BE71096123456769",
  "BA393385804800211234",
  "BR1500000000000010932840814P2",
  "BG18RZBB91550123456789",
  "CR23015108410026012345",
  "HR1723600001101234565",
  "CY21002001950000357001234567",
  "CZ5508000000001234567899",
  "DK9520000123456789",
  "DO22ACAU00000000000123456789",
  "SV43ACAT00000000000000123123",
  "EE471000001020145685",
  "FO9264600123456789",
  "FI1410093000123458",
  "FR7630006000011234567890189",
  "GE60NB0000000123456789",
  "DE91100000000123456789",
  "GI04BARC000001234567890",
  "GR9608100010000001234567890",
  "GL8964710123456789",
  "GT20AGRO00000000001234567890",
  "HU93116000060000000012345676",
  "IS030001121234561234567890",
  "IQ20CBIQ861800101010500",
  "IE64IRCE92050112345678",
  "IL170108000000012612345",
  "IT60X0542811101000000123456",
  "JO71CBJO0000000000001234567890",
  "KZ563190000012344567",
  "XK051212012345678906",
  "KW81CBKU0000000000001234560101",
  "LV97HABA0012345678910",
  "LB92000700000000123123456123",
  "LI7408806123456789012",
  "LT601010012345678901",
  "LU120010001234567891",
  "MK07200002785123453",
  "MT31MALT01100000000000000000123",
  "MR1300020001010000123456753",
  "MU43BOMM0101123456789101000MUR",
  "MD21EX000000000001234567",
  "MC5810096180790123456789085",
  "ME25505000012345678951",
  "NL02ABNA0123456789",
  "NO8330001234567",
  "PK36SCBL0000001123456702",
  "PS92PALS000000000400123456702",
  "PL10105000997603123456789123",
  "PT50002700000001234567833",
  "QA54QNBA000000000000693123456",
  "RO09BCYP0000001234567890",
  "LC14BOSL123456789012345678901234",
  "SM76P0854009812123456789123",
  "ST23000200000289355710148",
  "SA4420000001234567891234",
  "RS35105008123123123173",
  "SC52BAHL01031234567890123456USD",
  "SK8975000000000012345671",
  "SI56192001234567892",
  "ES7921000813610123456789",
  "SE7280000810340009783242",
  "CH5604835012345678009",
  "TL380010012345678910106",
  "TN5904018104004942712345",
  "TR320010009999901234567890",
  "UA903052992990004149123456789",
  "AE460090000000123456789",
  "GB98MIDL07009312345678",
  "VG21PACG0000000123456789"]

tooLongIbans = map (++ "1") validIbans

wrongRemainderIban = map (\x -> (init x) ++ [(int2let (let2int (last x) + 2))]) validIbans

{-
 We test our validation function with a list of valid IBANs, one for each country.
 We run falsification tests on the same numbers, but altered as to invalidate them.
 Both the tests and the falsification tests return answers that indicate the correctness of our code.
 To generate automated tests, we'd need to generate valid IBAN numbers. The most straightforward way
 would be to use a different implementation of our own code, which kind of undermines the purpose of
 testing; a mistake in our validation code would be indistinguishable from a mistake in the generation code.
 We could match the results of our own validator against online validators, but then still the issue of generating IBANs remain.
 Time: 1.5h
-}
-----------------------------------------------------------------------------------

main = do 
    putStrLn "--== EXERCISE 1: DISTRIBUTION ==--" 
    distributionTest

    putStrLn "\n--== EXERCISE 2: TRIANGLES ==--"
    quickCheck triangleTest

    putStrLn "\n--== EXERCISE 3: PROPERTY STRENGTH ==--"
    print mySortedPropertiesStrings
    putStrLn "Testing the property sorter"
    quickCheck sorterTest

    putStrLn "\n--== EXERCISE 4: PERMUTATIONS ==--"
    putStrLn "Manual permutation test:"
    manualPermutationTests
    putStrLn "Automated permutation test:"
    quickCheck isPermutationTest
    putStrLn "Automated falsification permutation test:"
    quickCheck falsifyPermutations

    putStrLn "\n--== EXERCISE 5: DERANGEMENT ==--"
    putStrLn "Manual derangement test:"
    quickCheck testDerangement
    putStrLn "Automated derangement test:"
    quickCheck quickCheckDerangement

    putStrLn "\n--== EXERCISE 6: ROT13 ==--"
    quickCheck testRot13

    putStrLn "\n--== EXERCISE 7: IBAN ==--"
    putStrLn "Valid IBAN test:"
    quickCheck testValidIbans
    putStrLn "Falsification IBAN test: too long IBANs"
    quickCheck testTooLongIbans
    putStrLn "Falsification IBAN test: wrong remainder IBANs"
    quickCheck testWrongRemainderIban
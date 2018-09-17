module Workshop2 where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck
import Text.Show.Functions
import qualified Data.Map as Map


infix 1 --> 
(-->) :: Bool -> Bool -> Bool

p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

forany :: [a] -> (a -> Bool) -> Bool
forany = flip any

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
pythagoreanTriplets = [(a, b, c)  | 
                       c          <- [1..], 
                       b          <- [1..c], 
                       a          <- [1..c], 
                       a^2 + b^2  == c^2,
                       a          < b]

otherTriangles :: [(Integer, Integer, Integer)]
otherTriangles = [(a, b, c) | 
                  c         <- [1..], 
                  b         <- [1..c], 
                  a         <- [1..c], 
                  a^2 + b^2 /= c^2, 
                  a < b, 
                  b < c, 
                  c < (a + b)]

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

evenAndGT3Show = "(\\x -> even x && x > 3)"
evenOrGT3Show = "(\\x -> even x || x > 3)"
evenAndGT3OrEvenShow = "(\\x -> (even x && x > 3) || even x)"
evenShow = "even"

sortFunctions :: (Int -> Bool) -> (Int -> Bool) -> Ordering
sortFunctions p q
              | stronger [-10..10] p q = GT
              | weaker [-10..10] p q = LT
              | (weaker [-10..10] p q) && (stronger [-10..10] p q)  = EQ

properties = [(even, evenShow), (evenAndGT3, evenAndGT3Show), (evenOrGT3, evenOrGT3Show), (evenAndGT3OrEven, evenAndGT3OrEvenShow)]

sortProperties = sortBy (\x y -> sortFunctions (fst x) (fst y)) properties 

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

-- 6 -- 1 hour
-- ROT13 specification
-- ROT13 replaces each letter by its partner 13 characters further along the alphabet. For example, HELLO becomes URYYB (or, conversely, URYYB becomes HELLO again).
-- ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13) is a simple letter substitution cipher that replaces a letter with the 13th letter after it, in the alphabet.

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

-- Helper code to generate strings with only AlphaNum characters
genSafeChar :: Gen Char
genSafeChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

rot13Test :: SafeString -> Bool
rot13Test (SafeString text) = text == encode 13 (encode 13 text) 

-- -- 7 ~ 1.5 hour
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
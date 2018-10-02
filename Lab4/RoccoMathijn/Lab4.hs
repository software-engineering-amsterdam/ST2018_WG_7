module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Data.Tuple
import Data.Char

import Lecture4

-- == Exercise 1 == --
{-
The concepts in Ch4 are clear to me
-}

-- == Exercise 2 == --
{-
Time spend: 2 hours
-}

{-
Scratch implementation

This implementation has a max length of 30 for the set and chooses random integers between -30 and 30.
-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (-n,n))

randomSet :: IO (Set Int) 
randomSet = do
              listLength <- getRandomInt 30
              list  <- sequence [getRandomInt 30 | _ <- [0..abs(listLength)]]
              return (Set (nub list))

-- Quickcheck arbitrary implementation
instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = (\x -> Set (nub x)) <$> arbitrary

-- == Exercise 3 == --
{-
Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck.
(Deliverables: implementations, test properties, short test report, indication of time spent.)

Time spend ~ 2 hours
-}

{-
Returns the intersection of the two sets
-}
intersection                   :: Eq a => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set [x | x <- xs, elem x ys]

{-
Returns the union of the two sets
-}
union                   :: Eq a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (nub (xs ++ ys))

{-
Returns the difference of the two sets
-}
difference                   :: Eq a => Set a -> Set a -> Set a
difference (Set xs) (Set ys) = Set [x | x <- xs, not (elem x ys)]

{-
Test property of intersection used by quickCheck and mySetCheck
-}
intersectionTest                    :: Set Int -> Set Int -> Bool
intersectionTest (Set xs) (Set ys)  = all (\x -> elem x xs && elem x ys) zs where 
                                          Set zs = intersection (Set xs) (Set ys)

{-
Test property of union used by quickCheck and mySetCheck
-}
unionTest                    :: Set Int -> Set Int -> Bool
unionTest (Set xs) (Set ys)  = all (\x -> elem x zs) xs && all (\x -> elem x zs) ys where 
                                          Set zs = Lab4.union (Set xs) (Set ys)

{-
Test property of difference used by quickCheck and mySetCheck
-}
differenceTest                    :: Set Int -> Set Int -> Bool
differenceTest (Set xs) (Set ys)  = not $ any (\x -> elem x ys) zs where 
                                          Set zs = difference (Set xs) (Set ys)
{-
Tests the set operations with my own random set generator
-}
mySetCheck    :: (Set Int -> Set Int -> Bool) -> IO ()
mySetCheck f  = do 
                  aSets <- sequence [randomSet | _ <- [0..100]]
                  bSets <- sequence [randomSet | _ <- [0..100]]
                  let result = all (==True) [f a b | a <- aSets, b <- bSets]
                  if (result) then putStrLn "+++ OK, passed 100 tests." else putStrLn "--- Failed." 

{-
Test runner for exercise 3

Runs every test once with my own generator and then with the QuickCheck generator
-}
testExercise3 = do 
                  putStrLn "\n--== Exercise 3 - Set operations ==--"
                  putStr "\tIntersection\n\t"
                  mySetCheck intersectionTest
                  putStr "\t"
                  quickCheck intersectionTest
                  putStr "\tUnion\n\t"
                  mySetCheck unionTest
                  putStr "\t"
                  quickCheck unionTest
                  putStr "\tDifference\n\t"
                  mySetCheck differenceTest
                  putStr "\t"
                  quickCheck differenceTest

-- == Exercise 5 == --
{-
Time spend: 15 minutes
-}
type Rel a = [(a,a)]

symClos     :: Ord a => Rel a -> Rel a
symClos rel = nub (concat [[(a,b),(b,a)] | (a,b) <- rel])

-- == Exercise 6 == --
{-
Time spend: ~1 hour
-}
infixr 5 @@

(@@)    :: Eq a => Rel a -> Rel a -> Rel a
r @@ s  = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos     :: Ord a => Rel a -> Rel a
trClos rel | all (\x -> elem x rel) t = nub rel
           | otherwise                = trClos (t ++ rel) where
                                          t = rel @@ rel

-- == Exercise 7 == --
{-
Time spend: 1 hour
-}
testSymClos     :: Rel Int -> Bool
testSymClos xs  = all (\x -> elem (swap x) symmetricClosure) symmetricClosure where
                    symmetricClosure = symClos xs

testTrClos    :: Rel Int -> Bool
testTrClos xs = all (\x -> elem x trClosure) transatives where 
                  transatives = trClosure @@ trClosure
                  trClosure = trClos xs

testExercise7 = do
                  putStrLn "\n--== Exercise 7 ==--"
                  putStrLn "\tSymmetric Closure"
                  putStr "\t"
                  quickCheck testSymClos
                  putStrLn "\tTransative Closure"
                  putStr "\t"
                  quickCheck testTrClos

-- == Exercise 8 == --
exercise8Property     :: Rel Int -> Bool
exercise8Property rel = (trClos . symClos) rel == (symClos . trClos) rel

testExercise8 = do
                  putStr "\n--== Exercise 8 ==--\n\t"
                  quickCheck (expectFailure . exercise8Property)

-- -- == Exercise 9 == --
{-
Time spend: ~1 hour
-}
--https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator
join sep xs = foldr (\a b-> a ++ if b=="" then b else sep ++ b) "" xs

--https://stackoverflow.com/questions/14907600/how-to-replace-a-string-with-another-in-haskell
replace a b s@(x:xs) = if isPrefixOf a s
                     then b++replace a b (drop (length a) s)
                     else x:replace a b xs
replace _ _ [] = []

instance Show Statement where
  show (Ass var expr)       = (var ++ " = " ++ (show expr))
  show (Cond cond st1 st2)  = "if (" ++ (show cond) ++ ") {\n\t" ++ (replace "\n" "\n\t" (show st1)) ++ "\n} else {\n\t" ++ (replace "\n" "\n\t" (show st2)) ++ "\n}"
  show (Seq statements)     = join "\n" (map show statements)
  show (While cond st)      = "\nwhile (" ++ show cond ++ ") {\n\t" ++ (replace "\n" "\n\t" (show st)) ++ "\n}"

instance Show Expr where
  show (I int)              = show int
  show (V var)              = var
  show (Add expr1 expr2)    = "(" ++ (show expr1) ++ " + " ++ (show expr2) ++ ")"
  show (Subtr expr1 expr2)  = "(" ++ (show expr1) ++ " - " ++ (show expr2) ++ ")"
  show (Mult expr1 expr2)   = "(" ++ (show expr1) ++ " * " ++ (show expr2) ++ ")"

instance Show Condition where
  show (Prp var)        = var
  show (Eq expr1 expr2) = (show expr1) ++ " == " ++ (show expr2)
  show (Lt expr1 expr2) = (show expr1) ++ " < " ++ (show expr2)
  show (Gt expr1 expr2) = (show expr1) ++ " > " ++ (show expr2)
  show (Ng cond)        = "(" ++ "!" ++ (show cond) ++ ")"
  show (Cj conds)       = "(" ++ join " || " (map show conds) ++ ")"
  show (Dj conds)       = "(" ++ join " && " (map show conds) ++ ")"

data Token 
      = TokenI Int
      | TokenV Var
      | TokenAdd
      | TokenSubtr
      | TokenMult
      | TokenOP
      | TokenCP
      | TokenOC
      | TokenCC
      | TokenAss
      | TokenPrp Var
      | TokenEquiv
      | TokenLt
      | TokenGt
      | TokenNg
      | TokenCj
      | TokenDj
      | TokenWhile
      | TokenCond
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs)                  = TokenOP : lexer cs
lexer (')':cs)                  = TokenCP : lexer cs
lexer ('{':cs)                  = TokenOC : lexer cs
lexer ('}':cs)                  = TokenCC : lexer cs
lexer ('<':cs)                  = TokenLt : lexer cs
lexer ('>':cs)                  = TokenGt : lexer cs
lexer ('!':cs)                  = TokenNg : lexer cs
lexer ('=':'=':cs)              = TokenEquiv : lexer cs
lexer ('|':'|':cs)              = TokenCj : lexer cs
lexer ('&':'&':cs)              = TokenDj : lexer cs
lexer ('=':cs)                  = TokenAss : lexer cs
lexer ('+':cs)                  = TokenAdd : lexer cs
lexer ('-':cs)                  = TokenSubtr : lexer cs
lexer ('*':cs)                  = TokenMult : lexer cs
lexer ('w':'h':'i':'l':'e':cs)  = TokenWhile : lexer cs
lexer ('i':'f':cs)              = TokenCond : lexer cs
lexer (c:cs) | isAlpha c        = lexVar (c:cs)
lexer (x:_)                     = error ("unknown token: " ++ [x])

lexNum cs = TokenI (read num) : lexer rest
  where (num,rest) = span isDigit cs

lexVar cs = TokenV variable : lexer rest
  where (variable,rest) = span (not . isSpace) cs

lexPrp cs = TokenPrp variable : lexer rest
  where (variable,rest) = span (not . isSpace) cs

testExercise9 = do
                  putStrLn "\n--== Exercise 9 ==--\n"
                  putStrLn "Show statement:\n"
                  putStrLn (show fib)
                  putStrLn "\n Result of lexer: \n"
                  putStrLn $ show  $ lexer $ show fib

{-
Main test runner
-}
main = do 
        testExercise3
        testExercise7
        testExercise8
        testExercise9
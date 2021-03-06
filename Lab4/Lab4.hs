module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck
import Data.Char
import Lecture4
import SetOrd

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-------------------------------------------------------------------------------
-- == Assignment 1: haskell questions == --
{-
 No mayor questions came up, no questions came up that could not be answered amongst ourselves.
-}

-------------------------------------------------------------------------------
-- == Assignment 2: QuickCheck generator for sets == --

-- For testing purposes we limit the range of natural numbers
maxNatural :: Int
maxNatural = 30

getRandomNatural :: IO Int
getRandomNatural = getStdRandom (randomR (1, maxNatural))

getRandomInteger :: IO Int
getRandomInteger = getStdRandom (randomR (-maxNatural, maxNatural))

getRandomIntegers :: Int -> IO [Int]
getRandomIntegers 0 = return []
getRandomIntegers n = do
             x <- getRandomInteger
             xs <- getRandomIntegers (n - 1) 
             return (x:xs)

integerSetGeneratorFromScratch :: IO (Set Int)
integerSetGeneratorFromScratch = do
    n <- getRandomNatural
    xs <- getRandomIntegers maxNatural
    return (Set (nub xs))

-- Quickcheck arbitrary implementation
instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = (\x -> Set (nub x)) <$> arbitrary


-- Time: 2h

-------------------------------------------------------------------------------
-- == Assignment 3: set operations and tests == --
-- Set operations:
intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set r) (Set s) = Set (intersect r s)

unionSet' :: Eq a => Set a -> Set a -> Set a
unionSet' (Set r) (Set s) = Set (nub (union r s))

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set r) (Set s) = Set (r \\ s)

--Helper code:
uniqueElements :: (Eq a) => [a] -> Bool
uniqueElements []     = True
uniqueElements (x:xs) = not (elem x xs) && uniqueElements xs

prop_UniqueElements :: (Eq a) => Set a -> Bool
prop_UniqueElements (Set xs) = uniqueElements xs

-- Common properties:

-- Must check both ways otherwise we might validate a implementation that always returns
-- the empty set.
prop_Intersected :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Intersected (Set r) (Set s) (Set rs) = all (\x -> (elem x r) && (elem x s)) rs &&
                                            all (\x -> (not (elem x s)) --> (not (elem x rs))) r &&
                                            all (\x -> (not (elem x r)) --> (not (elem x rs))) s &&
                                            prop_UniqueElements (Set rs)
-- test properties: an intersection is at most as big as the biggest of r and s.
--                  must contain any element both in r and in s
--                  may not contain any element that is not both in r and s.

prop_Unioned :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Unioned (Set r) (Set s) (Set rs) = all (\x -> elem x rs) r && 
                                        all (\x -> elem x rs) s &&
                                        all (\x -> (elem x r) || (elem x s)) rs &&
                                        prop_UniqueElements (Set rs)
-- test properties: cardinality of intersection is at least as big as the biggest of r and s.
--                  must contain any element that is in r or in s
--                  may not contain any element not in r or in s.

prop_Differented :: Eq a => Set a -> Set a -> Set a -> Bool
prop_Differented (Set r) (Set s) (Set rs) = all (\x -> elem x s --> not(elem x rs)) r &&
                                            all (\x -> (elem x r)) rs &&
                                            prop_UniqueElements (Set rs)
-- test properties: cardinality of intersection is at most the sum of sizes of r and s
--                  must contain any element that is either in r or in s
--                  may not contain any element not in r or in s, but not both.


-- Build for testing own generator

testIt :: Eq a => [(Set a, Set a)] -> (Set a -> Set a -> Set a) -> (Set a -> Set a -> Set a -> Bool) -> Bool
testIt xs op p = and [ p (fst x) (snd x) (op (fst x) (snd x)) | x <- xs ]

-- QuickCheck testable propeties

prop_QuickCheckIntersected :: Set Int -> Set Int -> Bool
prop_QuickCheckIntersected r s = prop_Intersected r s rs
    where rs = intersectSet r s

prop_QuickCheckUnioned :: Set Int -> Set Int -> Bool
prop_QuickCheckUnioned r s = prop_Unioned r s rs
    where rs = unionSet' r s

prop_QuickCheckDifferented :: Set Int -> Set Int -> Bool
prop_QuickCheckDifferented r s = prop_Differented r s rs
    where rs = differenceSet r s

-- Scratch support functions

generateSetTuples :: Int -> IO [(Set Int, Set Int)]
generateSetTuples 0 = return []
generateSetTuples n = do
            r <- integerSetGeneratorFromScratch
            s <- integerSetGeneratorFromScratch
            xs <- generateSetTuples (n - 1) 
            return ((r, s):xs)

numberOfTests :: Int
numberOfTests = 100

showTest :: Int -> Bool -> String
showTest _ False = "--- Failed."
showTest n _     = "+++ OK, passed " ++ (show n) ++ " tests."

testAssignment3 = do
    putStrLn "\n--== Assignment 3: Set Operations ==--"

    putStrLn "\nGenerator from scratch tests"
    putStr "Intersection: \t"
    xs <- generateSetTuples numberOfTests
    putStrLn (showTest numberOfTests (testIt xs intersectSet prop_Intersected))
    putStr "Union: \t\t"
    xs <- generateSetTuples numberOfTests
    putStrLn (showTest numberOfTests (testIt xs unionSet' prop_Unioned))
    putStr "Difference: \t"
    xs <- generateSetTuples numberOfTests
    putStrLn (showTest numberOfTests (testIt xs differenceSet prop_Differented))

    putStrLn "\nQuickCheck tests"
    putStr "Intersection: \t"
    quickCheck prop_QuickCheckIntersected
    putStr "Union: \t\t"
    quickCheck prop_QuickCheckUnioned
    putStr "Difference: \t"
    quickCheck prop_QuickCheckDifferented


-- Our elaborate tests show that our functions work consistently, within tight constraints.
-- Time spent: 2:30

-------------------------------------------------------------------------------
-- == Assignment 4: haskell questions: 2nd edition == --
{-
 No questions arose that probably won't be answered by closer study of the material.
-}

-------------------------------------------------------------------------------
-- == Assignment 5: symmetric closure == --

type Rel a  = [(a, a)]

--create a list of an element of the relation with its reverse
--add all these lists together
--remove duplicates from this cumulative list

symClos :: Ord a => Rel a -> Rel a
symClos rs = (nub (concat [ [r, swap(r)] | r <- rs ]) )

-- Time: 10min

-------------------------------------------------------------------------------
-- == Assignment 6: transitive closure == --

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Transitive transformation
tr :: Ord a => Rel a -> Rel a
tr r = sort (nub ((r @@ r) ++ r))

-- Transitive closure
trClos :: Ord a => Rel a -> Rel a
trClos r = until (\s -> tr s == s) tr r

-- Time: 20min
-- Completion time varied heavily within our group.
-------------------------------------------------------------------------------
-- == Assignment 7: testing 5 & 6 == --

-- Symmetric Closure properties

-- Yes, quickcheck can be used.
-- 1:   For every relation in the original set both versions (the original (x,y)
--      and the mirrored (y,x)) must be present in the symmetric closure.
-- 2:   For every relation in the symmetric closure (x,y) either (x,y) or (y,x) must
--      be present in the original set.

-- Every relation in the original set must be present in its original form and in its
-- morrored form in the symmetric closure.
prop_SymmetricElementsInClosure :: Rel Int -> Bool
prop_SymmetricElementsInClosure r = all (\(x,y) -> elem (x,y) s && elem (y,x) s) r'
    where s = symClos r'
          r' = nub r

-- Every relation in the symmetric closure must be present in the same form or as its
-- mirrored counterpart in the original set.
prop_ClosureElementHaveOrigin :: Rel Int -> Bool
prop_ClosureElementHaveOrigin r = all (\(x,y) -> elem (x,y) r' || elem (y,x) r') s
    where s = symClos r'
          r' = nub r

-- Transitive closure properties

-- The transitive closure of a transitive closure is equal to the original transitive closure.
prop_TransitivesDontChange :: Rel Int -> Bool
prop_TransitivesDontChange r = r' == trClos r'
    where r' = trClos (nub r)

relToList :: Ord a => Rel a -> [a]
relToList r = sort (nub (concat [ [x,y] | (x,y) <- r]))

-- All elements of the original set must be present in the transitive closure
prop_TransitiveClosurePreservesElements :: Rel Int -> Bool
prop_TransitiveClosurePreservesElements r = relToList r' == relToList (trClos r')
    where r' = nub r

-- Determines for a non transtive closure for a given start element the
-- elements that can trasitively be reached from that element.
reachables :: Ord a => a -> Rel a -> [a]
reachables x r = sort (nub (concat ([ z:(reachables z (filter (/=(y,z)) r')) | (y,z) <- r', y == x])))
    where r' = nub r

prop_TransitiveConnections :: Rel Int -> Bool
prop_TransitiveConnections r = all (\(x,_) -> reachables x r' == [ z | (y,z) <- s, x == y]) r'
    where s = trClos r'
          r' = nub r

testAssignment7 = do
    putStrLn "\n--== Assignment 7: Testing Symmetric and Transitive Closure ==--"

    putStrLn "\nSymmetric Closure tests"
    putStr "Symmetric elements exist in closure: \t"
    quickCheck prop_SymmetricElementsInClosure
    putStr "Closure element have origin: \t\t"
    quickCheck prop_ClosureElementHaveOrigin

    putStrLn "\nTransitive Closure tests"
    putStr "Transitives don't change: \t\t"
    quickCheck prop_TransitivesDontChange
    putStr "Transitive closure preserves elements: \t"
    quickCheck prop_TransitiveClosurePreservesElements
    putStr "Transitive connections: \t\t"
    quickCheck prop_TransitiveConnections

-------------------------------------------------------------------------------
-- == Assignment 8: checking (R_r)^+ == (R^+)_r == --

isEqualTrSymSymTrHelper :: Ord a => Rel a -> Bool
isEqualTrSymSymTrHelper rs = trClos (symClos rs) == symClos (trClos rs)

isEqualTrSymSymTrInt :: Rel Int -> Bool
isEqualTrSymSymTrInt rs = isEqualTrSymSymTrHelper rs

isEqualTrSymSymTrStr :: Rel String -> Bool
isEqualTrSymSymTrStr rs = isEqualTrSymSymTrHelper rs

testAssignment8 = do
    putStrLn "\n-- == Assignment 8: checking (R^-1)^+ == (R^+)^-1 == --"
    quickCheck (expectFailure . isEqualTrSymSymTrInt)
    quickCheck (expectFailure . isEqualTrSymSymTrStr)
    putStrLn "The tests fail, as such we can conclude (R^-1)^+ /= (R^+)^-1"
    putStrLn "Counterexample: R=[(1,0)]: (R_r)^+ = [(1,0),(0,1),(1,1),(0,0)], (R^+)_r = [(1,0),(0,1)]"

-------------------------------------------------------------------------------
-- == Assignment 9: Show and parser for imperative Haskell == --
{-
Time spend: 3 hours.

We did not get to build the parser. The amount of work involved was too much. We tried to model the form parser
from Lab3 but that parser can read from left to right. To proper parse the imperative code that we get from our show 
function you have to build a proper parse tree and we did not have time for that. We do however have the lexer.

The testAssignment9 function shows the output of 'fib' given in Lecture4.hs. After that it shows what the lexer 
makes of it.
-}

{-
We first took join and replace function from Data.List.Utils but this would require installing the 'MissingH'
package. We did not want to bother the TA with installing this package so we took the code snippets below
from StackOverflow.
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

testAssignment9 = do
                  putStrLn "\n--== Exercise 9: Show and parser for imperative Haskell ==--\n"
                  putStrLn "Calling show on the fib Statement:\n"
                  putStrLn (show fib)
                  putStrLn "\n Result of lexer: \n"
                  putStrLn $ show  $ lexer $ show fib

-------------------------------------------------------------------------------
-- == Main == --

main = do
    testAssignment3
    testAssignment7
    testAssignment8
    testAssignment9
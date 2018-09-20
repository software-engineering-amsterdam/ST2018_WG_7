module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

parse' :: String -> Form
parse' = head . parse

-- ASSIGNMENT 1 - PROPOSITIONAL LOGIC --

combinedValues :: [Form] -> [Valuation]
combinedValues fs = genVals (nub (concatMap propNames fs))

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = all (\ v -> (evl v f) --> (evl v g)) (combinedValues [f, g])

equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> evl v f == evl v g) (combinedValues [f, g])

-- Time spent: 0:40

np :: Form
np = parse' "-1"

nnnp :: Form
nnnp = parse' "---1"

allwaysTrue :: Form
allwaysTrue = parse' "+(1 -1)"

allwaysFalse :: Form
allwaysFalse = parse' "*(1 -1)"

andFunction :: Form
andFunction = parse' "*(1 2)"

andAsOrFunction :: Form
andAsOrFunction = parse' "-+(-1 -2)"

-- Tests --

-- contradiction allwaysfalse
-- tautology allwaysTrue
-- equiv andFunction andAsOrFunction
-- entails andFunction allwaysTrue 
-- entails allwaysTrue andFunction

-- Time spent: 0:15


-- ASSIGNMENT 3 - CNF Converter --

negateProp :: (Name, Bool) -> Form
negateProp (x, True)  = Neg (Prop x)
negateProp (x, False) = Prop x

convert :: Form -> Form
convert f = Cnj [ Dsj [ negateProp p | p <- v ] | v <- allVals f, not (evl v f)]

cnf :: Form -> Form
cnf f | tautology f = Dsj [Prop n, Neg (Prop n)] 
      | otherwise   = convert (arrowfree f)
      where n = head (propNames f)

-- Tests

-- onlyNegateAtoms :: Form -> Bool
-- onlyNegateAtoms (Neg (Prop _) = True

testEquilelance :: Form -> Form -> IO ()
testEquilelance f g = do
    putStr ((show f) ++ " <=> " ++ (show g) ++ ": ")
    putStrLn (show (equiv f g))

showCNFs :: Form -> Form -> IO ()
showCNFs f g = do
    putStrLn ((show f) ++ " ==? " ++ (show g) ++ "\n")

simpleImplies :: Form
simpleImplies = parse' "(1 ==> 2)"

simpleEquiv :: Form
simpleEquiv = parse' "(1 <=> 1)"

wsExample1 :: Form
wsExample1 = parse' "---1"

cnfWsExample1 :: Form
cnfWsExample1 = parse' "-1"

wsExample2 :: Form
wsExample2 = parse' "-+(1 -2)"

cnfWsExample2 :: Form
cnfWsExample2 = parse' "*(-1 2)"

wsExample3 :: Form
wsExample3 = parse' "((1 ==> 2)<=>(-2 ==> -1))"

cnfWsExample3 :: Form
cnfWsExample3 = parse' "+(1 -1)"

main = do 
    -- testEquilelance simpleImplies (cnf simpleImplies)
    -- testEquilelance simpleEquiv (cnf simpleEquiv)

    testEquilelance wsExample1 (cnf wsExample1)
    showCNFs (cnf wsExample1) cnfWsExample1

    testEquilelance wsExample2 (cnf wsExample2)
    showCNFs (cnf wsExample2) cnfWsExample2

    testEquilelance wsExample3 (cnf wsExample3)
    showCNFs (cnf wsExample3) cnfWsExample3

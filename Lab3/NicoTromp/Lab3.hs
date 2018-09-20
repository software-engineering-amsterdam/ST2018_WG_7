module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

parse' :: String -> Form
parse' = head . parse

-- ASSIGNMENT 1 - PROPOSITIONAL LOGIC --

combineValues :: [Form] -> [Valuation]
combineValues fs = genVals (nub (concatMap propNames fs))

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = all (\ v -> (evl v f) --> (evl v g)) (combineValues [f, g])

equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> evl v f == evl v g) (combineValues [f, g])

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
negateProp (n, True)  = Neg (Prop n)
negateProp (n, False) = Prop n

createCNF :: Form -> Form
createCNF f = Cnj [ Dsj [ negateProp v | v <- vs ] | vs <- allVals f, not (evl vs f)]

removeSingleJunctions :: Form -> Form
removeSingleJunctions (Dsj fs) | length fs == 1 = removeSingleJunctions (head fs)
removeSingleJunctions (Cnj fs) | length fs == 1 = removeSingleJunctions (head fs)
removeSingleJunctions f                         = f

cnf :: Form -> Form
cnf f | tautology f     = Dsj [Prop n, Neg (Prop n)] 
      | contradiction f = Cnj [Prop n, Neg (Prop n)]
      | otherwise       = removeSingleJunctions (createCNF (arrowfree f))
      -- Just use the first name for tautologies and contradiction.
      where n = head (propNames f)

-- Post condition checks!

isLiteral :: Form -> Bool
isLiteral (Prop _)       = True
isLiteral (Neg (Prop _)) = True
isLiteral _              = False

isCNFClause :: Form -> Bool
isCNFClause f | isLiteral f = True
isCNFClause (Dsj fs)        = all isCNFClause fs
isCNFClause _               = False

isCNFConjunction :: Form -> Bool
isCNFConjunction f | isCNFClause f = True
isCNFConjunction (Cnj fs)       = True
isCNFConjunction _              = False

testCNFEquivelance :: Form -> Bool
testCNFEquivelance f = equiv f (cnf f)

testCNFConversion :: Form -> Bool
testCNFConversion f = (not (isCNFConjunction f)) --> (isCNFConjunction (cnf f))

--------------------------------------------------

instance Arbitrary Form where
    arbitrary = sized arbitrarySizedForm

arbitrarySizedForm    :: Int -> Gen Form
arbitrarySizedForm n  =  do formIndex <- choose (0, 8)
                            size <- choose (0, n `div` 2)
                            arbitraryForm <- arbitrarySizedForm (n `div` 4)
                            listOfArbitraryForms <- vectorOf size (arbitrarySizedForm (n `div` 4))
                            let form = [Prop 1,
                                        Prop 2,
                                        Prop 3,
                                        Prop 4,
                                        Neg arbitraryForm,
                                        Cnj (arbitraryForm : listOfArbitraryForms),
                                        Dsj (arbitraryForm : listOfArbitraryForms),
                                        Impl arbitraryForm arbitraryForm,
                                        Equiv arbitraryForm arbitraryForm
                                        ] !! formIndex
                            return form

-------------------------------------------------

-- Development tests.

testEquivelance :: Form -> Form -> IO ()
testEquivelance f g = do
    putStr ((show f) ++ " <=> " ++ (show g) ++ ": ")
    putStrLn (show (equiv f g))

showCNFs :: Form -> Form -> IO ()
showCNFs f g = do
    putStrLn ((show f) ++ " ==? " ++ (show g) ++ "\n")

simpleImplies :: Form
simpleImplies = parse' "(1 ==> 2)"

simpleEquiv :: Form
simpleEquiv = parse' "(1 <=> 1)"

prop :: Form
prop = parse' "1"

negProp :: Form
negProp = parse' "-1"

disj :: Form
disj = parse' "+(1 2)"

disjNeg :: Form
disjNeg = parse' "+(-1 -2)"

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
    testEquivelance simpleImplies (cnf simpleImplies)
    testEquivelance simpleEquiv (cnf simpleEquiv)
    testEquivelance prop (cnf prop)
    testEquivelance negProp (cnf negProp)
    testEquivelance disj (cnf disj)
    testEquivelance disjNeg (cnf disjNeg)

    putStrLn "\n-------------------------------------\n"

    testEquivelance wsExample1 (cnf wsExample1)
    showCNFs (cnf wsExample1) cnfWsExample1

    testEquivelance wsExample2 (cnf wsExample2)
    showCNFs (cnf wsExample2) cnfWsExample2

    testEquivelance wsExample3 (cnf wsExample3)
    showCNFs (cnf wsExample3) cnfWsExample3

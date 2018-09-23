module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Exception

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

testAssignment1 = do
    putStrLn "--== Assignment 1 - Propositional Logic ==--"  

-- Time spent: 0:40


-- ASSIGNENT 2 -  --
{-
  Tests the parser by checking if the printable output the form equals the
  printable output of the result of parsing the printable output the form
-}
parseTest :: Form -> Bool
parseTest f = show f == (show . head . parse . show) f

parseEmptyString = do 
                    putStrLn "'parse' should return '[]'' when parsing an empty string"
                    if (parse "" == []) then putStrLn "+++ OK" else putStrLn "--- Failed"

parseBogus = do 
              putStrLn "'parse' should throw an error when parsing bogus"
              catch (putStrLn (show $ parse "Bogus")) errorHandler
              where 
                errorHandler :: SomeException -> IO ()
                errorHandler = (\err -> putStrLn "+++ OK, 'parse' threw an error as expected")

testAssignment2 = do
    putStrLn "\n--== Assignment 2 - Propositional Testing ==--" 
    quickCheck parseTest
    parseEmptyString
    parseBogus

-- ASSIGNMENT 3 - CNF Converter --

-- Definitions for CNF conversion and tests.
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
isCNFConjunction (Cnj fs)          = all isCNFClause fs
isCNFConjunction _                 = False

-- Actual CNF convertion.

-- Constructs a Form containing only the property, negated if needed, depending on the actual
-- value.
negateProp :: (Name, Bool) -> Form
negateProp (n, True)  = Neg (Prop n)
negateProp (n, False) = Prop n

-- Creates for any non tautology or contradiction form its corresponding CNF.
-- We make use of the hint that is described in the last paragraph of workshop 3.
-- 'Hint: the negation of a row where the truth table gives false can be expressed as a disjunction. 
-- Take the conjunction of all these disjunctions.' When constructing the disjunction we make use of the 
-- fact that 'not(p and q)' is equivelent to '(not p) or (not q)'. If the actual value for a property is
-- 'false' it must be replaced by 'not false', this is all handled by the 'negateProp' function.
convertToCNF :: Form -> Form
convertToCNF f | isLiteral f   = f
convertToCNF f | isCNFClause f = f
convertToCNF f                 = Cnj [ Dsj [ negateProp v | v <- vs ] | vs <- allVals f, not (evl vs f)]

-- Converts any propositional form into its CNF.
-- Specal cases are tautology and contradiction. These can be converted into a standard CNF form. 
-- In order to ensure that these special cases use property names that are present in the form, one
-- of them it selected to create the corresponding CNF.
-- All other cases are converted into CNF by the 'convertToCNF' function.
cnf :: Form -> Form
cnf f | tautology f     = Dsj [Prop n, Neg (Prop n)] 
      | contradiction f = Cnj [Prop n, Neg (Prop n)]
      | otherwise       = convertToCNF (nnf (arrowfree f))
      -- Just use the first name for tautologies and contradictions.
      where n = head (propNames f)

-- Testing is done by ensuring that the generated form is not in CNF. Non-CNF forms are transformed
-- into CNF. Then CNF-form is checked to make sure it actually in CNF and is logicaly equivelant to
-- the original. This test relies on the random form generator from assignment 4.
testCNFConversion :: Form -> Bool
testCNFConversion f = (not (isCNFConjunction f)) --> isCNFConjunction f' && equiv f f'
                    where
                        f' = cnf f

testAssignment3 = do
    putStrLn "\n--== Assignment 3 - CNF Converter ==--" 
    quickCheck testCNFConversion

-- Time spent: 4:30


-- ASSIGNMENT 4 - Form generator --

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

testAssignment4 = do
    putStrLn "\n--== Assignment 4 - Form generation Testing ==--" 

-- ASSIGNMENT 5 - Bonus --
type Clause  = [Int]
type Clauses = [Clause]

cnf2cls                 :: Form -> Clauses
cnf2cls (Prop x)        = [[x]]
cnf2cls (Neg (Prop x))  = [[-x]]
cnf2cls (Cnj formList)  = [concat (concatMap cnf2cls formList)]
cnf2cls (Dsj formList)  = concatMap cnf2cls formList

example1 = cnf2cls (Cnj [Prop 5, Neg (Prop 6)])
example2 = cnf2cls (Dsj [Prop 4, Cnj [Prop 5, Neg (Prop 6)]])

testExample1 = show example1 == "[[5,-6]]"
testExample2 = show example2 == "[[4],[5,-6]]"

-- TEST RUNNER --
main = do
    testAssignment1
    testAssignment2
    testAssignment3
    testAssignment4
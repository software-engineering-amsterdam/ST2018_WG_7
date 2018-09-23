module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Exception

-- == Exercise 1 == --
{-
  Time spend: 1 hour
-}

{-
  Checks whether a form is a contradiction or not
-}
contradiction   :: Form -> Bool
contradiction f = not $ satisfiable f

{-
  Checks whether a form is a tautology or not
-}
tautology   :: Form -> Bool
tautology f =  all (\ v -> evl v f) (allVals f)

{-
  Checks whether form1 entails form2 or not
-}
entails :: Form -> Form -> Bool
entails f1 f2 = not $ any (\v -> evl v f1 && (not $ (evl v f2))) valuations
                where valuations = genVals $ propNames f1 ++ propNames f2

{-
  Checks whether form1 is equivalent to form2 or not
-}
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f1 f2

-- == Exercise 2 == --
{-
  Time spend: ~1 hour
-}

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
        
-- == Exercise 3 == --
{-
  Time spend:
-}

-- == Exercise 4 == --
{-
  Time spend: ~3 hours
-}

{-
  Random generation of forms
-}
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

-- == Exercise 5 == --
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

-- == Exercise Runner == --
{-
  Runs all the tests of the assignments above
-}
main = do
          putStrLn "--==Exercise 2==--"
          putStrLn "Testing the parsing function on 100 arbitrary forms"
          quickCheck parseTest
          parseEmptyString
          parseBogus


-- Copied code out of the group file to make ex 5 work:
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
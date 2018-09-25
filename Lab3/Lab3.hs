module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Exception

--------------------------------------------------
-- ASSIGNMENT 1 - PROPOSITIONAL LOGIC --

-- Generates all the permutations of values given a list of forms.
-- First a list is build with unique names, then all permutations
-- are genereted.
combinedValues :: [Form] -> [Valuation]
combinedValues fs = genVals (nub (concatMap propNames fs))

-- A contradiction is not stisfiable
contradiction :: Form -> Bool
contradiction = not . satisfiable

-- For a tautology all evaluations must be true
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- If for a certain combination of inputs the function f returns true this must also
-- be the case for function g. 
entails :: Form -> Form -> Bool
entails f g = all (\ v -> (evl v f) --> (evl v g)) (combinedValues [f, g])

-- For every compination of input values both functions f and g must return the same value.
equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> evl v f == evl v g) (combinedValues [f, g])

-- One way to check this using QuickCheck would be to generate forms
-- automatically. You could do this by starting with atoms and adding operators
-- with operands randomly on the left and right until we decide to stop generating.

-- We could use this method as well to easily generate logically entailing
-- propositions, because we could always add an operator and operand to a
-- proposition which logically entails the original proposition. This could be done
-- by adding "or P" to a proposition, which will either be logically equivalent,
-- or be logically entailed by the original proposition.

-- Using this method however, we are not proving that our definition is correct.
-- We are only showing that for the generated formulas, our definition was correct.

-- If we can show that we can generate any possible formula with our generator,
-- Then we will prove it as long as we keep testing ad infinitum.

-- When it comes to contradictions, One way of generating them would be to
-- ensure that we're always sampling from all the possible minimal contradictions
-- with the given number of literals. If this sub formula is connected with an
-- "and" operator to the rest of the formula, we can disregard the rest of the
-- formula when checking for contradiction. The smallest contradiction is for
-- one literal: p and -p

-- Any contradiction can be rewritten to contain a minimal contradiction
-- which is sampled from the minimal contradictions for the number of literals.
-- We might even say that any formula is either rewriteable to a minimal contradiction,
-- or have a minized contradiction connected to the rest of the formula with an and operator.

-- Thus we can generate contradictions by picking a number of literals,
-- generating the list of minimal contradictions, picking one, and adding whatever
-- else we want in the formula with an "and" operator.

-- Similarly, we could do this for tautologies. The minimal tautology set is the
-- minimal set of negations of the minimal contradiction set. The smallest
-- tautology is thus obtained as such: -(p and -p) = -p or p = p or -p.

-- Any tautology is a negation of a contradiction. This means that the previous
-- statements about a minimal contradiction connected with an and operator
-- to the rest of the formula becomes a minimal tautology connected with an or
-- operator connected to the negation of the rest of the formula.

-- When building the minimal contradiction set we can use a trick:
-- literals | contradictions
-- p        | p and -p
-- p, q     | p and -p
--            q and -q
--            (p and -p) and (q and -q)
--            (p and -p) or (q and -q)
-- p, q     | contradictions(p)
--            contradictions(q)
--            contradictions(p) and contradictions(q)
--            contradictions(p) or contradictions(q)

-- If we can rewrite any contradiction to a minimal contradiction connected
-- with an and to the rest of the formula, the element
-- "contradictions(p) and contradictions(q)" can be rewritten to
-- contradictions(p) and -rest of the formula-
-- or, contradictions(q) and -rest of the formula-.
-- These cases are already accounted for with the elements contradictions(p)
-- and contradictions(q).

-- It should be noted that we also run into other contradictions with two literals,
-- such as (p and -q) and (q and -p). This can be rewritten to
-- contradictions(p) and contradictions(q).
-- We will also encounter (p or (q and -q)) and (-p or (q and -q)). This can be
-- rewritten to contradictions(p) or contradictions(q).

-- With more literals, we will also encounter combinations between or's and and's
-- and contradictions of subsets of the literals. We don't need to generate these
-- cases, because when rewritten to CNF, we can apply the 'and trick' to reduce
-- the proposition to the contradiction of a single literal, and
-- -rest of the formula-.

-- Our set of minimal contradictions can thus be reduced to the contradictions
-- for the the individual literals, and any combination of them with or connectors.
-- We then define genMinimalContradictions:

genMinimalContradictions :: Int -> [Form]
genMinimalContradictions literalCount = genMinimalContradictionsInternal [1..literalCount]

smallestContradiction :: Int -> Form
smallestContradiction literal = Cnj [Prop literal, Neg (Prop literal)]

genMinimalContradictionsInternal :: [Int] -> [Form]
genMinimalContradictionsInternal literals | (length literals) == 1 = [smallestContradiction (head literals)]
                                          | otherwise              = [Dsj (concat [genMinimalContradictionsInternal [literal] | literal <- literals])] ++
                                                                     (nub (concat [genMinimalContradictionsInternal subset | subset <- subsequences literals,
                                                                              length subset == (length literals) - 1]))

genMinimalTautologies :: Int -> [Form]
genMinimalTautologies literalCount = [nnf (Neg a) | a <- genMinimalContradictions literalCount]

-- Tests

allwaysTrue :: Form
allwaysTrue = head (parse "+(1 -1)")

allwaysFalse :: Form
allwaysFalse = head (parse "*(1 -1)")

andFunction :: Form
andFunction = head (parse "*(1 2)")

andAsOrFunction :: Form
andAsOrFunction = head (parse "-+(-1 -2)")

testAssignment1 = do
    putStrLn "--== Assignment 1 - Propositional Logic ==--"  
    putStrLn ("Contradiction is allways false: " ++ (show (contradiction allwaysFalse)))
    putStrLn ("Tautology is allways true: " ++ (show (tautology allwaysTrue)))
    putStrLn ("Conjuntion is equivelent to its disjunction form: " ++ (show (equiv andFunction andAsOrFunction)))
    putStrLn ("The and-function entails allways true: " ++ (show (entails andFunction allwaysTrue )))

-- Time spent: 0:40

--------------------------------------------------
-- ASSIGNMENT 2 -  --
{-
Time spend: ~2 hours
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

testAssignment2 = do
    putStrLn "\n--== Assignment 2 - Propositional Testing ==--" 
    quickCheck parseTest
    parseEmptyString
    parseBogus

--------------------------------------------------
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
arbitrarySizedForm 0 = do 
                        n <- choose (0,3)
                        let forms = [Prop 1,
                                     Prop 2,
                                     Prop 3,
                                     Prop 4
                                     ]
                        return (forms !! n)
arbitrarySizedForm n  =  do formIndex <- choose (0, 8)
                            size <- choose (0, n `div` 2)
                            arbitraryForm <- arbitrarySizedForm (n `div` 4)
                            arbitraryForm2 <- arbitrarySizedForm (n `div` 4)
                            listOfArbitraryForms <- vectorOf size (arbitrarySizedForm (n `div` 4))
                            let form = [Prop 1,
                                        Prop 2,
                                        Prop 3,
                                        Prop 4,
                                        Neg arbitraryForm,
                                        Cnj (arbitraryForm : listOfArbitraryForms),
                                        Dsj (arbitraryForm : listOfArbitraryForms),
                                        Impl arbitraryForm arbitraryForm2,
                                        Equiv arbitraryForm arbitraryForm2
                                        ] !! formIndex
                            return form

rewriteToCnfTester :: Form -> Bool
rewriteToCnfTester f = equiv f cnfF && cnfChecker cnfF
                        where cnfF = rewriteToCnf f

testAssignment4 = do
    putStrLn "\n--== Assignment 4 - Form generation Testing ==--" 
    quickCheck rewriteToCnfTester

-- ASSIGNMENT 5 - SAT Solving --
{-
Time spend: ~1 hour
-}
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

testAssignment5 = do
    putStrLn "\n--== Assignment 5 - SAT solving ==--" 
    putStrLn $ "Test example 1: " ++ (show testExample1)
    putStrLn $ "Test example 2: " ++ (show testExample2)

-- TEST RUNNER --
main = do
    testAssignment1
    testAssignment2
    testAssignment3
    testAssignment4
    testAssignment5
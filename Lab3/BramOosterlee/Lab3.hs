module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--Assignment 1 time taken 00:25 + started at 09:40

evals :: Form -> [Bool]
evals form = [evl val form | val <- (allVals form)]

contradiction :: Form -> Bool
contradiction form = all (==False) (evals form)

tautology :: Form -> Bool
tautology form = all (==True) (evals form)

trueVals :: Form -> [Valuation] -> [Valuation]
trueVals form vals = [val | val <- vals, (evl val form1) == True]

contains :: [Valuation] -> [Valuation] -> Bool
contains a b = all (==True) [elem val a | val <- b]

 -- | logical entailment
getCombinedVals :: Form -> Form -> [Valuation]
getCombinedVals form1 form2 = (genVals (nub ((propNames form1) ++ (propNames form2))))

entailsInternal :: Form -> Form -> [Valuation] -> Bool
entailsInternal form1 form2 combinedVals = contains (trueVals form1 combinedVals) (trueVals form2 combinedVals)

entails :: Form -> Form -> Bool
entails form1 form2 = entailsInternal form1 form2 (getCombinedVals form1 form2)

 -- | logical equivalence
equivInternal :: Form -> Form -> [Valuation] -> Bool
equivInternal form1 form2 combinedVals = (entailsInternal form1 form2 combinedVals) && (entailsInternal form2 form1 combinedVals)

equiv :: Form -> Form -> Bool
equiv form1 form2 = equivInternal form1 form2 (getCombinedVals form1 form2)

-- *Lab3> tautology form1
-- True
-- *Lab3> tautology form2
-- False
-- *Lab3> contradiction form2
-- False
-- *Lab3> contradiction form3
-- False
-- *Lab3> contradiction form1
-- False
-- *Lab3> satisfiable form2
-- True
-- *Lab3> satisfiable form3
-- True
-- *Lab3> equiv form2 form3
-- True
-- *Lab3> entails form2 form3
-- True
-- *Lab3> entails form3 form2
-- True
-- *Lab3> entails form1 form2
-- True
-- *Lab3> entails form2 form1
-- True
-- *Lab3> equiv form1 form2
-- True

-- One way to check this using QuickCheck would be to generate forms
-- automatically. You could do this by starting with atoms and adding operators
-- with operands randomly on the left and right until we decide to stop generating.

-- We could use this method as well to easily generate logically entailing
-- propositions, because we could always add an operator and operand to a
-- proposition which logically entails the original proposition. This could be done
-- by adding "or P" to a proposition, which will either be logically equivalent,
-- or logically entail the original proposition.

-- Using this method however, we are not proving that our definition is correct.
-- We are only showing that for the generated formulas, our definition was correct.

-- We can use properties to show

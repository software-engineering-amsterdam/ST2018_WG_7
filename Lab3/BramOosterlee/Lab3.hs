module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--Assignment 1 started at 00:25

evals :: Form -> [Bool]
evals form = [evl val form | val <- (allVals form)]

contradiction :: Form -> Bool
contradiction form = all (==False) (evals form)

tautology :: Form -> Bool
tautology form = all (==True) (evals form)

trueVals :: Form -> [Valuation]
trueVals form = [val | val <- (allVals form1), (evl val form1) == True]

contains :: [Valuation] -> [Valuation] -> Bool
contains a b = all (==True) [elem val a | val <- b]

 -- | logical entailment
entails :: Form -> Form -> Bool
entails form1 form2 = contains (trueVals form1) (trueVals form2)

 -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv form1 form2 = (entails form1 form2) && (entails form2 form1)

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

-- TODO generate forms automatically
-- TODO generate forms which logically entail other forms

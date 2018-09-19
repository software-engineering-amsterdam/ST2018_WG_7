module Sjoerd_Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-----------------------------------------------------------------------------------------
-- ==EXERCISE 1: LECTURE DEFINITIONS== --

myfunc f = map (\ v -> evl v f) (allVals f)

-- satisfiable :: Form -> Bool
-- satisfiable f = any (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- -- | logical entailment 
-- entails :: Form -> Form -> Bool
-- entails f g = all (\ v ->)

-- -- | logical equivalence
-- equiv :: Form -> Form -> Bool




-----------------------------------------------------------------------------------------
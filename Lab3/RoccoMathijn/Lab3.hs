module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction   :: Form -> Bool
contradiction f = not $ satisfiable f

tautology   :: Form -> Bool
tautology f =  all (\ v -> evl v f) (allVals f)

---- | logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = not $ any (\v -> evl v f1 && (not $ (evl v f2))) valuations
                where valuations = genVals $ propNames f1 ++ propNames f2

--logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f1 f2

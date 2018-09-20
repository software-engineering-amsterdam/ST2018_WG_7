module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- ASSIGNMENT 1 - PROPOSITIONAL LOGIC --

contradiction :: Form -> Bool
contradiction f = not (tautology f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = all (\ v -> (evl v f) --> (evl v g)) (allVals f)

equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> evl v f == evl v g) (allVals f)

-- Time spent: 0:20

allwaysTrue :: Form
allwaysTrue = head (parse "+(1 -1)")

allwaysFalse :: Form
allwaysFalse = head (parse "*(1 -1)")

andFunction :: Form
andFunction = head (parse "*(1 2)")

andAsOrFunction :: Form
andAsOrFunction = head (parse "-+(-1 -2)")

-- Tests --

-- contradiction allwaysfalse
-- tautology allwaysTrue
-- equiv andFunction andAsOrFunction
-- entails andFunction allwaysTrue 
-- entails allwaysTrue andFunction

-- Time spent: 0:15
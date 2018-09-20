module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- ASSIGNMENT 1 - PROPOSITIONAL LOGIC --

contradiction :: Form -> Bool
contradiction f = not (any (\ v -> evl v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = all (\ v -> (evl v f) --> (evl v g)) (genVals ((propNames f ++ propNames g)))

equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> evl v f == evl v g) (genVals ((propNames f ++ propNames g)))

-- Time spent: 0:30

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
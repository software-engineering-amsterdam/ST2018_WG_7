module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

parse' :: String -> Form
parse' = head . parse

-- ASSIGNMENT 1 - PROPOSITIONAL LOGIC --

combinedValues :: [Form] -> [Valuation]
combinedValues fs = genVals (nub (concatMap propNames fs))

contradiction :: Form -> Bool
contradiction = not satisfiable

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = all (\ v -> (evl v f) --> (evl v g)) (combinedValues [f, g])

equiv :: Form -> Form -> Bool
equiv f g = all (\ v -> evl v f == evl v g) (combinedValues [f, g])

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
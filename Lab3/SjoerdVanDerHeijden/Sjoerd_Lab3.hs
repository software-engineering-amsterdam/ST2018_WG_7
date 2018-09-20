module Sjoerd_Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-----------------------------------------------------------------------------------------
-- ==EXERCISE 1: LECTURE DEFINITIONS== --

-- satisfiable :: Form -> Bool
-- satisfiable f = any (\ v -> evl v f) (allVals f)

{- Helper function: generates the complete allVals for all properties present
 in both f and g. E.g.: f contains p and q, g contains p and r, combineAllVals
 creates a valuation for p, q and r.
-}
combineAllVals :: Form -> Form -> [Valuation]
combineAllVals f g = genVals (nub (propNames f ++ propNames g))
-- More flexible implementation of the previous
combineAllVals2 :: [Form] -> [Valuation]
combineAllVals2 forms = genVals (nub (concatMap propNames forms))

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = and [not (evl v f) || evl v g | v <- combineAllVals f g ]

equiv :: Form -> Form -> Bool
equiv f g = and [evl v f == evl v g | v <- combineAllVals f g ]

main = do
    print (satisfiable( head (parse "+(1 2)")))
    print (contradiction( head (parse "*(1 -1)")))
    print (tautology( head (parse "+(1 -1)")))
    print (entails (head (parse "1")) (head (parse "1")))
    print (equiv (head (parse "1")) (head (parse "1")))

-- Time: 1h
-----------------------------------------------------------------------------------------
-- ==EXERCISE 3: REWRITE TO CNF== --

cnfConverter :: Form -> Form
cnfConverter f = nnf (arrowfree f)

cleanup (Cnj [f,f]) = f
cleanup (Dsj [f,f]) = f
cleanup (Dsj [Dsj [f,-f],g]) = Dsj [f,-f]
cleanup (Cnj [Cnj [f,-f],g]) = Cnj [f,-f]

-- "(1 ==> 2) <=> ((-2) ==> (-1))"

logicConverter :: Form -> Form
logicConverter (Cnj [(Dsj fs), gs]) = Dsj (map ( Cnj.( (flip(:)) [gs] ) ) fs)
logicConverter (Dsj [(Cnj fs), gs]) = Cnj (map ( Dsj.( (flip(:)) [gs] ) ) fs)
logicConverter (Cnj fs) = Neg ( Dsj (map (nnf.Neg) fs ))
logicConverter (Dsj fs) = Neg ( Cnj (map (nnf.Neg) fs ))

-- is there a Neg without a Prop
-- isCnf f = 
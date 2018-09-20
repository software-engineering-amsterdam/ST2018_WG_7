module Sjoerd_Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------
-- ==EXERCISE 2: REWRITE TO CNF== --

-- quickCheck using ex 4.

-------------------------------------------------------------------------------
-- ==EXERCISE 3: REWRITE TO CNF== --

cnfFirstCleanup :: Form -> Form
cnfFirstCleanup f = nnf (arrowfree f)

-- cnfCleanup :: Form -> Form
-- -- equivalence cleanup
-- cnfCleanup (Cnj (f:g:[])) | f == g = f
--                         | otherwise = Cnj [f,g]
-- cnfCleanup (Dsj [f,f]) = f
-- -- Tautology cleanup
-- cnfCleanup (Dsj [Dsj [f, Neg f], g]) = Dsj [f,-f]
-- cnfCleanup (Cnj [Dsj [f, Neg f], g]) = g
-- -- Contradiction cleanup
-- cnfCleanup (Cnj [Cnj [f, Neg f], g]) = Cnj [f,-f]
-- cnfCleanup (Dsj [Cnj [f, Neg f], g]) = g

-- logicConverter :: Form -> Form
-- logicConverter (Cnj [(Dsj fs), gs]) = Dsj (map ( Cnj.( (flip(:)) [gs] ) ) fs)
-- logicConverter (Dsj [(Cnj fs), gs]) = Cnj (map ( Dsj.( (flip(:)) [gs] ) ) fs)
-- logicConverter (Cnj fs) = Neg ( Dsj (map (nnf.Neg) fs ))
-- logicConverter (Dsj fs) = Neg ( Cnj (map (nnf.Neg) fs ))

-- is there a Neg without a Prop
-- isCnf f = 

-- "(1 ==> 2) <=> ((-2) ==> (-1))"
-- getVals :: Valuation -> [Form]
-- getVals v = [if snd val then  (Neg (Prop(fst val))) else Prop (fst val) | val <- v]

-- cnfGenerator :: Form -> Form
-- cnfGenerator f = head [Cnj[Dsj (getVals v)] | v <- allVals f, not (evl v f) ]

-- Gets the Valuations belonging to the rows of the truth table for f that result in False.
getTTFalseRows :: Form -> [Valuation]
getTTFalseRows f = [v |  v <- allVals f, not (evl v f) ]

-- Transforms properties from a row of the truth table into the format needed by the cnf format.
getProps :: Valuation -> [Form]
getProps v = [if snd val then (Neg (Prop(fst val))) else Prop (fst val) | val <- v]

-- Puts the properties in a disjunction
getDsjProps :: Valuation -> Form
getDsjProps v | length props > 1 = Dsj props
              | length props == 1 = head props
              | otherwise = Dsj [Prop 1, Neg (Prop 1)]
               where props = getProps v

-- Puts the disjunctions into a conjunction
getCnjProps :: [Valuation] -> Form
getCnjProps v | length props > 1 = Cnj props
              | length props == 1 = head props
              | otherwise = Cnj [Prop 1, Neg (Prop 1)]
               where props = [getDsjProps vals | vals <- v]

-- Rewrites a Form into a CNF form.
rewriteToCnf :: Form -> Form
rewriteToCnf f = getCnjProps (getTTFalseRows f)

-- "+(+(*(1 -2) *(-1 2)) *(*(-1 -2) -3))"
-------------------------------------------------------------------------------
-- ==EXERCISE 4: REWRITE TO CNF== --


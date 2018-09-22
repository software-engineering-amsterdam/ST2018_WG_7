module Sjoerd_Lab3 where

import Data.List
import Data.Typeable
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


-- Gets the Valuations belonging to the rows of the truth table for f that result in False.
getTTFalseRows :: Form -> [Valuation]
getTTFalseRows f = [v |  v <- allVals f, not (evl v f) ]

-- Transforms properties from a row of the truth table into the format needed by the cnf.
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
rewriteToCnf f | tautology f = Dsj [Prop 1, Neg (Prop 1) ]
               | contradiction f = Cnj [Prop 1, Neg (Prop 1) ]
               | cnfChecker (nnf (arrowFree f)) = (nnf (arrowFree f))
               | otherwise = getCnjProps (getTTFalseRows f)

-- Used to check whether there's a Cnj nested within a Dsj, which would violate CNF. Is there a better way to write this?
cnjInDsjChecker :: Form -> Bool
cnjInDsjChecker (Cnj f) = True
cnjInDsjChecker f = False

-- Checks whether a form is in CNF
cnfChecker :: Form -> Bool
cnfChecker (Neg (Prop f)) = True
cnfChecker (Prop f) = True
cnfChecker (Neg f) = False
cnfChecker (Impl f g) = False
cnfChecker (Equiv f g) = False
cnfChecker (Dsj []) = False
cnfChecker (Cnj []) = False
cnfChecker (Dsj fs) = and [ not (cnjInDsjChecker f) && cnfChecker f | f <- fs ]
cnfChecker (Cnj fs) = and [ cnfChecker f | f <- fs ]


-- "+(+(*(1 -2) *(-1 2)) *(*(-1 -2) -3))"
-- "(1 ==> 2) <=> ((-2) ==> (-1))"

-------------------------------------------------------------------------------
-- ==EXERCISE 4: RANDOM PROPERTY GENERATOR== --

-- instance Arbitrary a => Arbitrary (Form a) where
--   arbitrary =
--     sized arbitrarySizedForm

-- arbitrarySizedForm :: Arbitrary a => Int -> Gen (Form)
-- arbitrarySizedForm m = do
--   t <- arbitrary
--   n <- choose (0, m `div` 2)
--   ts <- vectorOf n (arbitrarySizedForm (m `div` 4))
--   return (Form t ts)
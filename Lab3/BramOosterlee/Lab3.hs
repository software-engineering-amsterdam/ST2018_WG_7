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
-- or be logically entailed by the original proposition.

-- Using this method however, we are not proving that our definition is correct.
-- We are only showing that for the generated formulas, our definition was correct.

-- If we can show that we can generate any possible formula with our generator,
-- Then we will prove it as long as we keep testing ad infinitum.

-- When it comes to contradictions, One way of generating them would be to
-- ensure that we're always sampling from all the possible minimal contradictions
-- with the given number of literals. If this sub formula is connected with an
-- "and" operator to the rest of the formula, we can disregard the rest of the
-- formula when checking for contradiction. The smallest contradiction is for
-- one literal: p and -p

-- Any contradiction can be rewritten to contain a minimal contradiction
-- which is sampled from the minimal contradictions for the number of literals.
-- We might even say that any formula is either rewriteable to a minimal contradiction,
-- or have a minized contradiction connected to the rest of the formula with an and operator.

-- Thus we can generate contradictions by picking a number of literals,
-- generating the list of minimal contradictions, picking one, and adding whatever
-- else we want in the formula with an "and" operator.

-- Similarly, we could do this for tautologies. The minimal tautology set is the
-- minimal set of negations of the minimal contradiction set. The smallest
-- tautology is thus obtained as such: -(p and -p) = -p or p = p or -p.

-- Any tautology is a negation of a contradiction. This means that the previous
-- statements about a minimal contradiction connected with an and operator
-- to the rest of the formula becomes a minimal tautology connected with an or
-- operator connected to the negation of the rest of the formula.

-- When building the minimal contradiction set we can use a trick:
-- literals | contradictions
-- p        | p and -p
-- p, q     | p and -p
--            q and -q
--            (p and -p) and (q and -q)
--            (p and -p) or (q and -q)
-- p, q     | contradictions(p)
--            contradictions(q)
--            contradictions(p) and contradictions(q)
--            contradictions(p) or contradictions(q)

-- If we can rewrite any contradiction to a minimal contradiction connected
-- with an and to the rest of the formula, the element
-- "contradictions(p) and contradictions(q)" can be rewritten to
-- contradictions(p) and -rest of the formula-
-- or, contradictions(q) and -rest of the formula-.
-- These cases are already accounted for with the elements contradictions(p)
-- and contradictions(q).

-- With more literals, we will also encounter combinations between or's and and's
-- and contradictions of subsets of the literals. We don't need to generate these
-- cases, because when rewritten to DNF, they will end up looking as such:
-- contradictions(p) or contradictions(q) or contradictions(r) or ...
-- Which is accounted for even after applying the 'and trick'.

-- Our set of minimal contradictions can thus be reduced to the contradictions
-- for the the individual literals, and any combination of them with or connectors.
-- We then define genMinimalContradictions:

genMinimalContradictions :: Int -> [Form]
genMinimalContradictions literalCount = genMinimalContradictionsInternal [1..literalCount]

smallestContradiction :: Int -> Form
smallestContradiction literal = Cnj [Prop literal, Neg (Prop literal)]

genMinimalContradictionsInternal :: [Int] -> [Form]
genMinimalContradictionsInternal literals | (length literals) == 1 = [smallestContradiction (head literals)]
                                          | otherwise              = [Dsj (concat [genMinimalContradictionsInternal [literal] | literal <- literals])] ++
                                                                     (nub (concat [genMinimalContradictionsInternal subset | subset <- subsequences literals,
                                                                              length subset == (length literals) - 1]))

genMinimalTautologies :: Int -> [Form]
genMinimalTautologies literalCount = [nnf (Neg a) | a <- genMinimalContradictions literalCount]

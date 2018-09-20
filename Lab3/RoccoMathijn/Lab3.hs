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

instance Arbitrary Form where
    arbitrary = sized arbitrarySizedForm

arbitrarySizedForm    :: Int -> Gen Form
arbitrarySizedForm n  =  do formIndex <- choose (0, 8)
                            size <- choose (0, n `div` 2)
                            arbitraryForm <- arbitrarySizedForm (n `div` 4)
                            listOfArbitraryForms <- vectorOf size (arbitrarySizedForm (n `div` 4))
                            let form = [Prop 1,
                                        Prop 2,
                                        Prop 3,
                                        Prop 4,
                                        Neg arbitraryForm,
                                        Cnj (arbitraryForm : listOfArbitraryForms),
                                        Dsj (arbitraryForm : listOfArbitraryForms),
                                        Impl arbitraryForm arbitraryForm,
                                        Equiv arbitraryForm arbitraryForm
                                        ] !! formIndex
                            return form

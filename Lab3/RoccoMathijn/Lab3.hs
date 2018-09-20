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

-- == Exercise 2 == --
{-
  Tests the parser by checking if the printable output of a form equals the
  printable output of the result of parsing the printable output of a form
-}
parseTest :: Form -> Bool
parseTest f = show f == (show . head . parse . show) f

-- == Exercise 4 == --
{-
  Random generation of forms
-}
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

-- == Exercise Runner == --
main = do
          putStrLn "--==Exercise 2==--"
          quickCheck parseTest
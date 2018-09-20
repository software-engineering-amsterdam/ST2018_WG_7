module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- == Exercise 1 == --
{-
  Time spend: 1 hour
-}

{-
  Checks whether a form is a contradiction or not
-}
contradiction   :: Form -> Bool
contradiction f = not $ satisfiable f

{-
  Checks whether a form is a tautology or not
-}
tautology   :: Form -> Bool
tautology f =  all (\ v -> evl v f) (allVals f)

{-
  Checks whether form1 entails form2 or not
-}
entails :: Form -> Form -> Bool
entails f1 f2 = not $ any (\v -> evl v f1 && (not $ (evl v f2))) valuations
                where valuations = genVals $ propNames f1 ++ propNames f2

{-
  Checks whether form1 is equivalent to form2 or not
-}
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f1 f2

-- == Exercise 2 == --
{-
  Time spend: ~20 minutes
-}

{-
  Tests the parser by checking if the printable output the form equals the
  printable output of the result of parsing the printable output the form
-}
parseTest :: Form -> Bool
parseTest f = show f == (show . head . parse . show) f

-- == Exercise 4 == --
{-
  Time spend: ~3 hours
-}

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
{-
  Runs all the tests of the assignments above
-}
main = do
          putStrLn "--==Exercise 2==--"
          putStrLn "Testing the parsing function on 100 arbitrary forms"
          quickCheck parseTest
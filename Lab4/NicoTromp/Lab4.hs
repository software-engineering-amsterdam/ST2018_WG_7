module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck

import SetOrd

-- For testing purposes we limit the range of natural numbers
maxNatural :: Int
maxNatural = 10

getRandomNatural :: IO Int
getRandomNatural = getStdRandom (randomR (1, maxNatural))

getRandomIntegers :: Int -> IO [Int]
getRandomIntegers 0 = return []
getRandomIntegers n = do
             x <- getStdRandom (randomR (-maxNatural, maxNatural))
             xs <- getRandomIntegers (n - 1) 
             return (x:xs)

convertToSet :: [Int] -> Set Int
convertToSet [] = emptySet
convertToSet (x:xs) = insertSet x (convertToSet xs)

integerSetGenerator :: IO (Set Int)
integerSetGenerator = do
    n <- getRandomNatural
    xs <- getRandomIntegers n
    return (convertToSet (nub xs))


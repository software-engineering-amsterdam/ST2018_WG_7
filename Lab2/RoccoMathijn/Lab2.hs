module Workshop2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
(-->) :: Bool -> Bool -> Bool

p --> q = (not p) || q

data Shape =  NoTriangle | Equilateral| Isosceles  | Rectangular | Other 
              deriving (Eq,Show)

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

-- 1 -- ~ 1 hour
quartileTest :: IO ()
quartileTest =  do
                  randomFloats <- probs 10000
                  let grouped = groupBy (\x y -> floor(x * 4) == floor(y * 4)) (sort randomFloats)
                  putStrLn (show (map length grouped))



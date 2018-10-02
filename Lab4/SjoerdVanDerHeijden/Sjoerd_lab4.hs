module Sjoerd_Lab4 where

import Data.List
import Data.Typeable
import Data.Tuple
import System.Random
import Test.QuickCheck
import Lecture4
import SetOrd
import Data.List.Split
import Data.Char

-------------------------------------------------------------------------------
-- == Assignment 1: haskell questions == --

-------------------------------------------------------------------------------
-- == Assignment 2: QuickCheck generator for sets == --

instance (Arbitrary a, Eq a) => Arbitrary (Set a) where
    arbitrary = do 
                x <- arbitrary
                return (Set (nub (x)))
-- Time: 30 

randomListGen :: Int -> Int -> IO [Int]
randomListGen 0 maxN = return []
randomListGen n maxN = do
        p <- getStdRandom (randomR ((-maxN), maxN))
        ps <- randomListGen (n-1) maxN
        return (p:ps)

maxLen :: Int
maxLen = 30
maxN :: Int
maxN = 30

randomSetGen :: IO (Set Int)
randomSetGen = do
        size <- getStdRandom (randomR (0, maxLen))
        randomList <- randomListGen size maxN
        return (Set (nub(randomList)))

-- Time: 30min, even though it's mostly copy pasted.

-------------------------------------------------------------------------------
-- == Assignment 3: set operations and tests == --
-- Helper code:

setLength :: Set a -> Int
setLength (Set r) = length r

-- Checks whether all elements of r are in s
setAllElem :: Eq a => Set a -> Set a -> Bool
setAllElem (Set r) (Set s) = all ((flip elem) s) r

-- Why do I even have to do this? I should not be forced to do this
getListFromSet :: Set a -> [a]
getListFromSet (Set r) = r

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)


-- Operation definitions
setIntersect :: Eq a => Set a -> Set a -> Set a
setIntersect (Set r) (Set s) = Set [x | x <- r, elem x s]
-- test properties: an intersection is at most as big as the biggest of r and s.
--                  must contain any element both in r and in s
--                  may not contain any element that is not both in r and s.

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion (Set r) (Set s) = Set (nub (r++s))
-- test properties: lenght of intersection is at least as big as the biggest of r and s.
--                  must contain any element that is in r or in s
--                  may not contain any element not in r or in s.

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set r) (Set s) = Set (concat [ x | x <- (groupBy (==) (sort (r++s))), length x == 1])
-- test properties: lenght of intersection is at most the sum of sizes of r and s
--                  must contain any element that is either in r or in s
--                  may not contain any element not in r or in s, but not both.


-- Helper code for testing 
intersectTestHelper :: Ord a => Set a -> Set a -> Bool
intersectTestHelper r s = setLength r >= setLength intersection 
                        && setLength s >= setLength intersection
                        && setAllElem intersection r
                        && setAllElem intersection s
                            where intersection = setIntersect r s

unionTestHelper :: Ord a => Set a -> Set a -> Bool
unionTestHelper r s = setLength r <= setLength union 
                    && setLength s <= setLength union
                    && setAllElem r union
                    && setAllElem s union
                        where union = setUnion r s

differenceTestHelper :: Ord a => Set a -> Set a -> Bool
differenceTestHelper (Set r) (Set s) = length r + length s >= setLength difference 
                                     && and [xor (elem t r) (elem t s) | t <- getListFromSet difference]
                                            where difference = setDifference (Set r) (Set s)


-- My own tester. Runs 100 tests
myCheckFuncs :: IO ()
myCheckFuncs = do
        rs <- sequence [randomSetGen | _ <- [0..100]]
        ss <- sequence [randomSetGen | _ <- [0..100]]
        if and [intersectTestHelper r s | r <- rs, s <- ss] then putStr "\tpassed 100 tests" else putStr "failed a test"
        putStrLn " for setIntersect"
        if and [unionTestHelper r s | r <- rs, s <- ss] then putStr "\tpassed 100 tests" else putStr "failed a test"
        putStrLn " for setUnion"
        if and [differenceTestHelper r s | r <- rs, s <- ss] then putStr "\tpassed 100 tests" else putStr "failed a test"
        putStrLn " for setDifference"

-- QuickTest test functions.
intersectTestInt :: Set Int -> Set Int -> Bool
intersectTestInt r s = intersectTestHelper r s
intersectTestStr :: Set String -> Set String -> Bool
intersectTestStr r s = intersectTestHelper r s

unionTestInt :: Set Int -> Set Int -> Bool
unionTestInt r s = unionTestHelper r s
unionTestStr :: Set String -> Set String -> Bool
unionTestStr r s = unionTestHelper r s

differenceTestInt :: Set Int -> Set Int -> Bool
differenceTestInt r s = differenceTestHelper r s
differenceTestStr :: Set String -> Set String -> Bool
differenceTestStr r s = differenceTestHelper r s


ass3Tester = do
    putStrLn "\n-- == Assignment 3: set operations == --"
    putStrLn "Testing with my own set generator"
    myCheckFuncs
    putStrLn "Testing with quickCheck:"
    putStr "Testing setIntersect\n\t"
    quickCheck intersectTestInt
    putStr "\t"
    quickCheck intersectTestStr
    putStr "Testing setUnion\n\t"
    quickCheck unionTestInt
    putStr "\t"
    quickCheck unionTestStr
    putStr "Testing setDifference\n\t"
    quickCheck differenceTestInt
    putStr "\t"
    quickCheck differenceTestStr

-- Time: 2h

-------------------------------------------------------------------------------
-- == Assignment 4: haskell questions: 2nd edition == --


-------------------------------------------------------------------------------
-- == Assignment 5: symmetric closure == --

type Rel a = [(a,a)]

-- Every element of rs and their reverse is put into a list; this list is then 
-- the symmetric closure of rs.
symClos :: Ord a => Rel a -> Rel a
symClos rs = (nub (concat [ [r, swap(r)] | r <- rs ]) )

-- Time: 5min

-------------------------------------------------------------------------------
-- == Assignment 6: transetive closure == --

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{- (rs @@ rs) gives the transitions of rs, but not yet any transitions within
 itself or between itself and rs. To do that recursion is required, until
 the list does not change anymore; at that moment the closure is found.
-}
trClos :: Ord a => Rel a -> Rel a 
trClos rs = if rs == ss then rs else trClos ss
    where
        ss = sort ( nub( (rs @@ rs) ++ rs) )

-- Time: 20min
-------------------------------------------------------------------------------
-- == Assignment 7: testing 5 & 6 == --

symClosTestHelper :: Ord a => Rel a -> Bool
symClosTestHelper rs = length rs <= length rSymClos && and [elem (swap r) rSymClos | r <- rs]
    where rSymClos = symClos rs
-- test properties: every element and the reverse of every element in rs must be in rs
--                  but no more than that, and no duplicates
--                  the symmetric closure of rs is at least as big as rs.

-- 
trClosTestHelper :: Ord a => Rel a -> Bool 
trClosTestHelper rs = length rsNubbed <= length rsTrClos 
                   && all ((flip elem) rsTrClos ) rsNubbed 
                   && all ((flip elem) rsTrClos ) (rsTrClos@@rsTrClos)
                    where rsTrClos = trClos (nub rs)
                          rsNubbed = nub rs
-- test properties: every element in the @@ of the transitive closure of rs with itself, must be contained in the transitive closure of rs
--                  transitive closure of rs is at least as big as rs

symClosTesterInt :: Rel Int -> Bool
symClosTesterInt rs = symClosTestHelper rs

symClosTesterStr :: Rel String -> Bool
symClosTesterStr rs = symClosTestHelper rs

trClosTesterInt :: Rel Int -> Bool
trClosTesterInt rs = trClosTestHelper rs

trClosTesterStr :: Rel String -> Bool
trClosTesterStr rs = trClosTestHelper rs


ass7Tester = do
    putStrLn "\n-- == Assignment 7: testing 5 & 6 == --"
    putStr "Testing symClos\n\t"
    quickCheck symClosTesterInt
    putStr "\t"
    quickCheck symClosTesterStr
    putStr "Testing symClos\n\t"
    quickCheck trClosTesterInt
    putStr "\t"
    quickCheck trClosTesterStr

-- Time: 40min

-------------------------------------------------------------------------------
-- == Assignment 8: checking (R_r)^+ == (R^+)_r == --

isEqualTrSymSymTrHelper :: Ord a => Rel a -> Bool
isEqualTrSymSymTrHelper rs = trClos (symClos rs) == symClos (trClos rs)

isEqualTrSymSymTrInt :: Rel Int -> Bool
isEqualTrSymSymTrInt rs = isEqualTrSymSymTrHelper rs

isEqualTrSymSymTrStr :: Rel String -> Bool
isEqualTrSymSymTrStr rs = isEqualTrSymSymTrHelper rs

ass8Tester = do
    putStrLn "\n-- == Assignment 8: checking (R^-1)^+ == (R^+)^-1 == --"
    quickCheck (expectFailure . isEqualTrSymSymTrInt)
    quickCheck (expectFailure . isEqualTrSymSymTrStr)
    putStrLn "The tests fail, as such (R^-1)^+ /= (R^+)^-1"
    putStrLn "Counterexample: R=[(1,0)]: (R_r)^+ = [(1,0),(0,1),(1,1),(0,0)], (R^+)_r = [(1,0)]"

-- Time: 30min

-------------------------------------------------------------------------------
-- == Main == --
main = do
    ass3Tester
    ass7Tester
    ass8Tester





-------------------------------------------------------------------------------
-- Temporary testing space

-- instance Show Statement where
--   show (Ass var expr)       = (var ++ " = " ++ (show expr))
--   show (Cond cond st1 st2)  = "if (" ++ (show cond) ++ ") {\n\t" ++ (replace "\n" "\n\t" (show st1)) ++ "\n} else {\n\t" ++ (replace "\n" "\n\t" (show st2)) ++ "\n}"
--   show (Seq statements)     = join "\n" (map show statements)
--   show (While cond st)      = "\nwhile (" ++ show cond ++ ") {\n\t" ++ (replace "\n" "\n\t" (show st)) ++ "\n}"

-- instance Show Expr where
--   show (I int)              = show int
--   show (V var)              = var
--   show (Add expr1 expr2)    = "(" ++ (show expr1) ++ " + " ++ (show expr2) ++ ")"
--   show (Subtr expr1 expr2)  = "(" ++ (show expr1) ++ " - " ++ (show expr2) ++ ")"
--   show (Mult expr1 expr2)   = "(" ++ (show expr1) ++ " * " ++ (show expr2) ++ ")"

-- instance Show Condition where
--   show (Prp var)        = var
--   show (Eq expr1 expr2) = (show expr1) ++ " == " ++ (show expr2)
--   show (Lt expr1 expr2) = (show expr1) ++ " < " ++ (show expr2)
--   show (Gt expr1 expr2) = (show expr1) ++ " > " ++ (show expr2)
--   show (Ng cond)        = "(" ++ "!" ++ (show cond) ++ ")"
--   show (Cj conds)       = "(" ++ join " || " (map show conds) ++ ")"
--   show (Dj conds)       = "(" ++ join " && " (map show conds) ++ ")"

-- testExercise9 = do
--                   putStr "\n--== Exercise 9 ==--\n"
--                   putStrLn (show fib)

-- simpleParser "("++str++"||"++  ++")" = 


-- simpleParser "("++str = simpleParser str
-- simpleParser str1 ++ "||" ++ str2 = 
-- simpleParser ")"++str = 

stripWhitespace :: String -> String
stripWhitespace str = [ char | char <- str, not (isSpace char)]

-- simpleParser "("++str++")" = simpleParser str
-- simpleParser str | elem '|' str = Dj [ simpleParser cond | cond <- splitOn "||" str ]
--                  | elem '&' str = Dj [ simpleParser cond | cond <- splitOn "&&" str ]
--                  | elem '+' str = (Add (splitOn "+" str)!!0 (splitOn "+" str)!!1)
--                  | elem '-' str = (Subtr (splitOn "-" str)!!0 (splitOn "-" str)!!1)
--                  | elem '*' str = (Mult (splitOn "*" str)!!0 (splitOn "*" str)!!1)
--                  | elem '<' str = (Mult (splitOn "<" str)!!0 (splitOn "<" str)!!1)
--                  | elem '>' str = (Mult (splitOn ">" str)!!0 (splitOn ">" str)!!1)
--                  | otherwise = Prp str
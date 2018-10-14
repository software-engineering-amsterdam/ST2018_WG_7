module Workshop6 where

import Data.Char
import Data.List

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

leafCount :: Blt a -> Int
leafCount (Leaf _)   = 1
leafCount (Node l r) = leafCount l + leafCount r

mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf x)   = Leaf (f x)
mapB f (Node l r) = Node (mapB f l) (mapB f l)

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

count :: Tree a -> Int
count (T _ []) = 1
-- count (T _ xs) = 1 + foldr (\x z -> z + count x) 0 xs
count (T _ xs) = 1 + foldl (\z x -> z + count x) 0 xs

depth :: Tree a -> Int
depth (T _ []) = 1
depth (T _ ts) = foldl max 0 (map depth ts) + 1

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x []) = T (f x) []
mapT f (T x xs) = T (f x) (map (mapT f) xs)

collect :: Tree a -> [a]
collect (T x []) = [x]
collect (T x xs) = x : (concatMap collect xs)


foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' (T _ []) = 1
count' t        = foldT (\_ ns -> 1 + sum ns) t

depth' :: Tree a -> Int
depth' (T _ []) = 1
depth' t        = foldT (\_ ns -> 1 + maximum (0:ns)) t

collect' :: Tree a -> [a]
collect' = foldT (\n ns -> n:concat ns)

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f = foldT (\n ns -> T (f n) ns)
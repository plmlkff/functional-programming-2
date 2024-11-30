{-# LANGUAGE DeriveGeneric #-}
module AVLSet
(
    AVLTree(Empty),
    insert,
    delete,
    map',
    filter',
    foldr'',
    foldl'',
    toList
) where

import GHC.Generics (Generic)
import Data.Validity
import Test.Validity (GenValid)

data AVLTree t = Empty
              | Node t (AVLTree t) (AVLTree t) deriving (Show, Eq, Generic)

instance (Validity t) => Validity (AVLTree t)
instance (GenValid t) => GenValid (AVLTree t)

left :: Ord t => AVLTree t -> AVLTree t
left (Node _ l _) = l

right :: Ord t => AVLTree t -> AVLTree t
right (Node _ _ r) = r

value :: Ord t => AVLTree t -> t
value (Node v _ _) = v

balanceFactor :: Ord t => AVLTree t -> Int
balanceFactor Empty = 0
balanceFactor node = height (left node) - height (right node)

height :: Ord t => AVLTree t -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

balanceLL :: Ord t => AVLTree t -> AVLTree t
balanceLL (Node v (Node vl tl ul) u) = Node vl tl (Node v ul u)

balanceLR :: Ord t => AVLTree t -> AVLTree t
balanceLR (Node v (Node vl tl (Node vlr tlr ulr)) u) = Node vlr (Node vl tl tlr) (Node v ulr u)

balanceRL :: Ord t => AVLTree t -> AVLTree t
balanceRL (Node v t (Node vr (Node vrl trl url) ur)) = Node vrl (Node v t trl) (Node vr url ur)

balanceRR :: Ord t => AVLTree t -> AVLTree t
balanceRR (Node v t (Node vr tr ur)) = Node vr (Node v t tr) ur

toList :: Ord t => AVLTree t -> [t]
toList Empty = []
toList (Node v l r) = toList l ++ [v] ++ toList r

getMin :: Ord t => AVLTree t -> t
getMin tree = if left tree == Empty then value tree else getMin $ left tree

insert ::  Ord t => t -> AVLTree t -> AVLTree t
insert v Empty = Node v Empty Empty
insert newV (Node v l r)
    | newV == v = Node v l r
    | newV < v && balanceFactor (Node v ti r) ==  2 && newV < value l = balanceLL (Node v ti r)
    | newV < v && balanceFactor (Node v ti r)  ==  2 && newV > value l = balanceLR (Node v ti r)
    | newV > v && balanceFactor (Node v l ui) == -2 && newV < value r = balanceRL (Node v l ui)
    | newV > v && balanceFactor (Node v l ui) == -2 && newV > value r = balanceRR (Node v l ui)
    | newV < v  = Node v ti r
    | newV > v  = Node v l ui
        where ti = insert newV l
              ui = insert newV r

-- Balanced delete
delete :: Ord t => AVLTree t -> t -> AVLTree t
delete Empty b = Empty
delete (Node v Empty Empty) d = if v == d then Empty else Node v Empty Empty
delete (Node v t Empty) d = if v == d then t else Node v t Empty
delete (Node v Empty u) d = if v == d then u else Node v Empty u
delete (Node v t u) d
    | v == d = Node mu t dmin
    | v > d && abs (balanceFactor $ Node v dt u) < 2 = Node v dt u
    | v < d && abs (balanceFactor $ Node v t du) < 2 = Node v t du
    | v > d && (balanceFactor $ Node v (left u) (right u)) < 0 = balanceRR (Node v dt u) 
    | v < d && (balanceFactor $ Node v (left t) (right t)) > 0 = balanceLL (Node v t du)
    | v > d = balanceRL (Node v dt u)
    | v < d = balanceLR (Node v t du)
        where dmin = delete u mu
              dt   = delete t d
              du   = delete u d
              mu   = getMin u


map' :: (Ord t, Ord b) => (t -> b) -> AVLTree t -> AVLTree b
map' f rootNode = mapByList f (toList rootNode) Empty
    where 
        mapByList :: (Ord t, Ord b ) => (t -> b) -> [t] -> AVLTree b -> AVLTree b
        mapByList f [] resTree = resTree
        mapByList f (x:xs) resTree = mapByList f xs ( insert (f x) resTree)

filter' :: Ord t => (t -> Bool) -> AVLTree t -> AVLTree t
filter' f sourceTree = filterByList f (toList sourceTree) Empty
    where 
        filterByList :: Ord t => (t -> Bool) -> [t] -> AVLTree t -> AVLTree t
        filterByList f [] resultTree = resultTree
        filterByList f (x : xs) resultTree
            | f x = filterByList f xs (insert x resultTree) 
            | otherwise = filterByList f xs resultTree

foldr'' :: Ord t => (t -> t -> t) -> t -> AVLTree t -> t
foldr'' f start tree = foldrByList f start (toList tree)
    where 
        foldrByList :: (t -> t -> t) -> t -> [t] -> t
        foldrByList _ start [] = start
        foldrByList f start [x] = f start x
        foldrByList f start (x : xs) = f x (foldrByList f start xs)


foldl'' :: Ord t => (t -> t -> t) -> t -> AVLTree t -> t
foldl'' f start tree = foldlByList f start (toList tree)
    where 
        foldlByList :: (t -> t -> t) -> t -> [t] -> t
        foldlByList _ start [] = start
        foldlByList f start (x : xs) = foldlByList f (f start x) xs
        
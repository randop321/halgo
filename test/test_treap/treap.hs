{-
Treap with three basic operators: insert, delete and lower_bound.
Duplicated elements are removed automatically.
WRN: the range of prior (the random number in treap) in this code is (-oo, 1e9)
-}


module Treap
( 
Tree,
newEmptyTree,
isEmptyTree,
getValue, 
getPrior, 
getLeft, 
getRight,
insert,
delete,
lower_bound
) where

data Tree = Nil | Node Int Int Tree Tree deriving (Show)
--                     value prior left_son right_son

newEmptyTree :: Tree
{-# INLINE newEmptyTree #-}
newEmptyTree = Nil

create :: (Int, Int) -> Tree
{-# INLINE create #-}
create (value, prior) = Node value prior Nil Nil

isEmptyTree :: Tree -> Bool
{-# INLINE isEmptyTree #-}
isEmptyTree Nil = True
isEmptyTree _ = False

getValue :: Tree -> Int
{-# INLINE getValue #-}
getValue (Node v _ _ _) = v

getPrior :: Tree -> Int
{-# INLINE getPrior #-}
getPrior (Node _ p _ _) = p

getLeft :: Tree -> Tree
{-# INLINE getLeft #-}
getLeft (Node _ _ left _) = left

getRight :: Tree -> Tree
{-# INLINE getRight #-}
getRight (Node _ _ _ right) = right


rotateLeft :: Tree -> Tree
rotateLeft (Node v p (Node v1 p1 left1 right1) right)
    | p <= p1 = Node v p (Node v1 p1 left1 right1) right
    | otherwise = Node v1 p1 left1 (Node v p right1 right)


rotateRight :: Tree -> Tree
rotateRight (Node v p left (Node v1 p1 left1 right1))
    | p <= p1 = Node v p left (Node v1 p1 left1 right1)
    | otherwise = Node v1 p1 (Node v p left left1) right1
    

-- Insert an element v with prior p. If v is existed in the tree before, do nothing. 
insert :: (Int, Int) -> Tree -> Tree
insert (v, p) Nil = create (v, p)
insert (v, p) (Node v1 p1 left right)
    | v < v1 = rotateLeft (Node v1 p1 (insert (v,p) left) right)
    | v == v1 = (Node v1 p1 left right)
    | otherwise = rotateRight (Node v1 p1 left (insert (v,p) right))


update :: Tree -> Tree -> Tree
{-# INLINE update #-}
update t Nil = t
update t1 t2 = t2


-- Find the minimum value greater or equal to the key, return the corresponding node or Nil.
lower_bound :: Int -> Tree -> Tree
lower_bound key Nil = Nil
lower_bound key (Node v p left right)
    | key > v = lower_bound key right
    | otherwise = update (Node v p left right) (lower_bound key left)
    

-- Delete the root node of the tree, return the root of new tree.
deleteRoot :: Tree -> Tree
deleteRoot Nil = Nil
deleteRoot (Node v p Nil Nil) = Nil
deleteRoot (Node v p left Nil) = delete v (rotateLeft (Node v p left Nil) )
deleteRoot (Node v p Nil right) = delete v (rotateRight (Node v p Nil right) )
deleteRoot (Node v p left right) 
    | getPrior left < getPrior right = delete v (rotateLeft (Node v p left right))
    | otherwise = delete v (rotateRight (Node v p left right))


-- Delete the element key and return the toor of new tree. If the key is not existed in the tree, do nothing.
delete :: Int -> Tree -> Tree
delete key Nil = Nil
delete key (Node v p left right)
    | key < v = Node v p (delete key left) right
    | key > v = Node v p left (delete key right)
    | otherwise = deleteRoot (Node v 1000000000 left right) -- WRN: we use 1e9 to represent the maxmimum value of prior
    

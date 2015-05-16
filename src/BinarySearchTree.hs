module BinarySearchTree where

data BST = Empty | Node Int (BST) (BST) deriving Show

makeBST :: [Int] -> BST
makeBST [] = Empty
makeBST (x:xs) = Node x (makeBST (filter (\y -> y < x) xs)) (makeBST (filter (\y -> y > x) xs))

bstHeight :: BST -> Int
bstHeight Empty = 0
bstHeight (Node _ left right) = 1 + max (bstHeight left) (bstHeight right)

bstSum :: BST -> Int
bstSum Empty = 0
bstSum (Node x left right) = x + bstSum left + bstSum right

bstFindElem :: BST -> Int -> Bool
bstFindElem Empty _ = False
bstFindElem (Node x left right) y | (x == y) = True
                                  | (x > y) = bstFindElem right y
                                  | (x < y) = bstFindElem left y
                                  | otherwise = False

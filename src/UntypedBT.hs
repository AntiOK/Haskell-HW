module UntypedBT where

data UntypedBT a = Empty | Node a (UntypedBT a) (UntypedBT a) deriving (Show)

makeUntypedBST :: UntypedBT a -> b
makeUntypedBST t = case t of 
                        Empty -> (undefined t)
                        Node _ _ _ -> (undefined t)


treeHight :: UntypedBT a -> Int
treeHight Empty = 0
treeHight (Node _ left right) = 1 + max (treeHight left) (treeHight right)

tmap :: (a -> b) -> UntypedBT a -> UntypedBT b
tmap _ Empty = Empty
tmap f (Node x left right) = Node (f x) (tmap f left) (tmap f right)

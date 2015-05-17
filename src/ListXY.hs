module ListXY where

data ListXY x y = Empty | Elem x (ListXY y x) deriving (Show)

makeListXY :: ListXY x y -> a
makeListXY listXY = case listXY of 
                             Empty -> (undefined listXY)
                             Elem _ _ -> (undefined listXY)

sizeOfListXY :: ListXY x y -> Int
sizeOfListXY Empty = 0
sizeOfListXY (Elem _ xs) = 1 + sizeOfListXY xs

dmap :: (a -> c) -> (b -> d) -> ListXY a b -> ListXY c d
dmap _ _ Empty = Empty
dmap f1 f2 (Elem x xs) = Elem (f1 x) (dmap f2 f1 xs)
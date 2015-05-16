module ConcatMap where

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap _ [] = []
myConcatMap f (x:xs) = f x ++ myConcatMap f xs

myFoldlConcatMap :: (a -> [b]) -> [a] -> [b]
myFoldlConcatMap f x = foldl(\y tl -> y ++ f tl) [] x

myFoldrConcatMap :: (a -> [b]) -> [a] -> [b]
myFoldrConcatMap f x = foldr(\y tl -> f y ++ tl) [] x

main :: IO()
main = do
    print (myConcatMap (replicate 3) [1,3,5])
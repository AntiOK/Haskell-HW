module MapFold where


myFoldlMap :: (a -> b) -> [a] -> [b]
myFoldlMap f x = foldl (\tl y -> tl ++ [f y]) [] x

myFoldrMap :: (a -> b) -> [a] -> [b]
myFoldrMap f x = foldr (\y tl -> f y : tl) [] x

plusOne x = x + 1

main :: IO()
main = do
    print (myFoldlMap plusOne [1, 2, 3])

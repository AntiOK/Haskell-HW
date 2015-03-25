import Control.Monad

toGrowth :: Int -> Int
toGrowth 0 = 1
toGrowth n = if (mod n 2 == 0) then toGrowth (n-1) + 1 else toGrowth (n-1) * 2

main :: IO ()
main = do
    n <- readLn :: IO Int
    str <- replicateM n getLine
    let 
        ans = map toGrowth (map (\x -> read x) str)
    mapM_ print ans
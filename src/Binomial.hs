module Binomial where

evalBinom :: Int -> Int -> Int -> Int
evalBinom a b n = binom a b n n

binom :: Int -> Int -> Int -> Int -> Int
binom a b n 0 = (binomCoeff n 0) * (a^n)
binom a b n k = (binomCoeff n k) * (a^(n-k)) * (b^k) + binom a b n (k-1)

binomCoeff :: Int -> Int -> Int
binomCoeff n k = round(fromIntegral (factorial n)/(fromIntegral ((factorial k) * (factorial (n-k)))))


fact :: Int -> Int -> Int
fact 0 acc = acc
fact n acc = fact (n-1) (acc*n)

factorial :: Int -> Int
factorial n = fact n 1

main = do
    print (evalBinom 2 3 12)
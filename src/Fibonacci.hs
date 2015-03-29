module Fibonacci where

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

fibonacci'' :: Int -> Int
fibonacci'' n = fib n 1 0 1

fib :: Int -> Int -> Int -> Int -> Int
fib n i fib1 fib2 | (n == i) = fib1
                  | otherwise = fib n (i+1) (fib1+fib2) fib1
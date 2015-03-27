module Factorial where

factorial :: Int -> Int
factorial 0 = 1
factorial n = factorial (n-1) * n

fact' :: Int -> Int -> Int
fact' 0 acc = acc
fact' n acc = fact' (n-1) (acc*n)

factorial' :: Int -> Int
factorial' n = fact' n 1

factorial'' :: Int -> Int
factorial'' n = product [1..n]

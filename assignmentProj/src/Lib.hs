module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "hello world"

double :: Num a => a -> a
double x = x + x

t :: Int -> Int
t 0 = 0
t n = n + t (n-1)

sq :: Int -> Int
sq n = n * n

s :: Int -> Int
s n = t n +  t (n-1)

--s(10) evaluates to 100
--sq(10) evaluates to 100 too
--Thus s(10) == sq(10) evaluates to True
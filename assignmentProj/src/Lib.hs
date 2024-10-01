module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "hello world"

double :: Num a => a -> a
double x = x + x
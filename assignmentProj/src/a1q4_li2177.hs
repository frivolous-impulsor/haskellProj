module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "hello world"

data Form = T | F | Not Form | And Form Form | Or Form Form | Implies Form Form | Iff Form Form


eval :: Form -> Bool
eval T = True
eval F = False
eval (Not f) = not (eval f)
eval (And a b) = eval a && eval b
eval (Or a b) = eval a || eval b
eval (Implies a b)
    | eval b = True
    | not (eval a) = True
    | otherwise = False
eval (Iff a b) = eval (Implies a b) && eval (Implies b a)
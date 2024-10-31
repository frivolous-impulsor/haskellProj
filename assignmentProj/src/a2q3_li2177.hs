{-# LANGUAGE GADTs #-}

data Expr a where
    ExBool :: Bool -> Expr Bool
    ExInt :: Int -> Expr Int
    ExPlus :: Expr Int -> Expr Int -> Expr Int
    ExMinus :: Expr Int -> Expr Int -> Expr Int
    ExTimes :: Expr Int -> Expr Int -> Expr Int
    ExPower :: Expr Int -> Expr Int -> Expr Int
    ExLessThan :: Expr Int -> Expr Int ->  Expr Bool
    ExGreaterThan :: Expr Int -> Expr Int ->  Expr Bool
    ExEquals :: Expr Int -> Expr Int ->  Expr Bool

eval :: Expr a -> a
eval e = case e of
    ExBool a -> a
    ExInt a -> a
    ExPlus a b -> (eval a) + (eval b)
    ExMinus a b -> (eval a) - (eval b)
    ExTimes a b -> (eval a) * (eval b)
    ExPower a b -> (eval a) ^ (eval b)  --for integer power only
    ExLessThan a b -> (eval a) < (eval b)
    ExGreaterThan a b -> (eval a) > (eval b)
    ExEquals a b -> (eval a) == (eval b)
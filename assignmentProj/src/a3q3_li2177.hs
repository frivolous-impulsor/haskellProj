data Term = Var String
    | Lambda String Term
    | App Term Term
    deriving (Show, Eq)

isRedex :: Term -> Bool
isRedex (App (Lambda x t1) t2) = True
isRedex t = False

redexes :: Term -> [Term]
redexes (Var x)= []
redexes (Lambda x t) = []
redexes (App a b) 
    | isRedex (App a b) = [(App a b)] ++ (redexes a) ++ (redexes b)
    | otherwise = (redexes a) ++ (redexes b)

sub :: Term -> String -> Term -> Term   --sub all occurance of y in the precending term with term t, a beta reduction
sub (Var x) y t 
    | x == y = t
    | otherwise = Var x
sub (Lambda x t1) y t2
    | x == y = Lambda x t1  --if var x is bound, substituting x should be avoided
    | otherwise = Lambda x (sub t1 y t2)
sub (App t1 t2) x t3 = App (sub t1 x t3) (sub t2 x t3)

eval :: Term -> Term 
eval (Var x) = Var x
eval (Lambda x t) = Lambda x t
eval (App (Lambda x t1) t2) = sub t1 x (eval t2)
eval (App t1 t2) = App t1 t2


--(λab.a)(λcd.d)
t :: Term
t = App (Lambda "a" (Lambda "b" (Var "a")))   (Lambda "c" (Lambda "d" (Var "d"))) 
--(λpq.pqp)
result :: Term
result = eval (App (Lambda "p" (Lambda "q" (App (Var "p") (App (Var "q") (Var "p"))))) t)


main :: IO()
main = print result
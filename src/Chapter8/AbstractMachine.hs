module Chapter8.AbstractMachine where

--Exercise 9  added Multiplication
data Expr = Val Int | Add Expr Expr | Mul Expr Expr
            deriving (Show)

value :: Expr -> Int
--value (Val n)   = n
--value (Add x y) = value x + value y
value e = eval e []

type Cont = [Op]
data Op = EVALADD Expr | EVALMUL Expr | ADD Int | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVALADD y : c)
eval (Mul x y) c = eval x (EVALMUL y : c)

exec :: Cont -> Int -> Int
exec []           n = n 
exec (EVALADD y : c) n = eval y (ADD n : c)
exec (EVALMUL y : c) n = eval y (MUL n : c)
exec (ADD n : c)  m = exec c (n+m)
exec (MUL n : c)  m = exec c (n*m)

--Exercise 5
folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a 
folde f g h (Val n) = f n
folde f g h (Add e1 e2) = g (folde f g h e1) (folde f g h e2)
folde f g h (Mul e1 e2) = h (folde f g h e1) (folde f g h e2)

--Exercise 6
eval' :: Expr -> Int
eval' = folde id (+) (*)

size :: Expr -> Int
size = folde (const 1) (+) (+)


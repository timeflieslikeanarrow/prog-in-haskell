module Chapter8.TautologyChecker where

import Chapter8.Base

import Chapter7.Base

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Disj Prop Prop            --Exercise 8
          | Equiv Prop Prop
          deriving (Show)

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s 
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Disj p q)  = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Disj p q)  = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
--bools n = map (reverse . map conv . make n . int2bin) range
--          where range     = [0..(2^n) - 1]
--                make n bs = take n (bs ++ repeat 0)
--                conv 0    = False
--                conv 1    = True

bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n - 1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
module Chapter12.Exercise where

--Exercise 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b 
  fmap g Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x)  (fmap g r)

--Exercise 2
--instance Functor ((->) a) where
--  -- fmap :: ((->) b c) -> ((->) a b) -> ((->) a c)
--  fmap = (.)

--Exercise 3
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  --fmap :: (a -> b) :: ZipList a -> ZipList b 
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  --pure :: a -> ZipList
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b 
  (Z gs) <*> (Z xs) = Z [ g x | (g, x) <- zip gs xs]

module Chapter14.Code where

import Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)
  
--fold :: Monoid a => Tree a -> a 
--fold (Leaf x)   = x
--fold (Node l r) = fold l `mappend` fold r

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x)   = x
  fold (Node l r) = fold l `mappend` fold r 

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b 
  foldMap f (Leaf x)   = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r 

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b 
  foldr f v (Leaf x)   = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x)   = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n-1) else Nothing

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r

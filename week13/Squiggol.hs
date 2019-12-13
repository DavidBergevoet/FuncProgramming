{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Squiggol
where

-- Base functor.

class (Functor f) => Base f where
  type Rec f :: *

  inn  ::  f (Rec f) -> Rec f  -- tying the recursive knot
  out  ::  Rec f -> f (Rec f)  -- untying the recursive knot

-- Fold and unfold.

fold :: (Base f) => (f a -> a) -> (Rec f -> a)
fold alg = consume
  where consume = alg . fmap consume . out 

unfold :: (Base f) => (a -> f a) -> (a -> Rec f)
unfold coalg = produce
  where produce = inn . fmap produce . coalg

-- Para- and apomorphism.

splitRight :: (a -> b) -> a -> (a, b)
splitRight f = \x -> (x, f x)

para :: (Base f) => (f (Rec f,res) -> res) -> Rec f -> res
para alg = consume
  where consume = alg . fmap (splitRight consume) . out

joinRight :: (a -> b) -> (Either b a -> b)
joinRight  f = \case Left  l -> l
                     Right r -> f r

apo :: (Base f) => (seed -> f (Either (Rec f) seed)) -> seed -> Rec f
apo coalg = produce
  where produce = inn . fmap (joinRight produce) . coalg

-- Base functor for Haskell's list datatype.

data LIST elem list  =  NIL | CONS elem list
   deriving Show
   
instance Functor (LIST elem) where
  fmap _f  (NIL)       = NIL
  fmap f   (CONS x xs) = CONS x (f xs)

instance Base (LIST elem) where
  type Rec (LIST elem) = [elem]

  inn (NIL)        =  []
  inn (CONS x xs)  =  x : xs

  out []           =  NIL
  out (x : xs)     =  CONS x xs

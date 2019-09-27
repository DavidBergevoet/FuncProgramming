-- David Bergevoet s1043736
-- Harm van der Heide s1047460

module Calculus
where

data Primitive
  =  Sin  -- trigonometric: sine
  |  Cos                          -- cosine
  |  Exp  -- exponential
  deriving (Show)

infixl 6 :+:
infixl 7 :*:
infixr 9 :.:

data Function
  =  Const Rational         -- constant function
  |  Id                     -- identity
  |  Prim Primitive         -- primitive function
  |  Function :+: Function  -- addition of functions
  |  Function :*: Function  -- multiplication of functions
  |  Function :.: Function  -- composition of functions
  deriving (Show)



apply    :: Function -> (Double -> Double)
apply (Id) = \y->y
apply (Const x) = \y->fromRational x
apply (Prim Sin) = \y->sin y
apply (Prim Cos) = \y->cos y
apply (Prim Exp) = \y->exp y
apply (x1 :+: x2) = \y->(apply(x1) y) + (apply(x2) y)
apply (x1 :*: x2) = \y->(apply(x1) y) * (apply(x2) y)
apply (x1 :.: x2) = \y->apply(x1) (apply x2 y)
 
 
derive   :: Function -> Function
derive (Const x) = Const 0
derive (Id) = Const 1
derive (Prim Sin) = Prim Cos
derive (Prim Cos) = (Const (-1)) :*: (Prim Sin)
derive (Prim Exp) = Prim Exp
derive (x1 :+: x2) = (derive x1) :+: (derive x2)
derive (Id :*: Id) = (Const 2) :*: Id
derive (Const x1 :*: x2) = (Const x1) :*: (derive x2)
derive (x1 :*: x2) = (derive x1) :*: (derive x2)



simplify :: Function -> Function
simplify (Const x1) = Const x1
simplify (Id) = Id
simplify ((Const x1) :+: (Const x2)) = Const(x1 + x2)
simplify ( Id :+: (Const x1)) = Id :+: (Const x1)

simplify ((Const x1) :+: x2) 
 | x1 == 0 = simplify x2
 | otherwise = (Const x1) :+: (simplify x2)

simplify (x1 :+: x2) = simplify(x1) :+: simplify(x2)
 
simplify(Id :+: Id) = (Const 2) :*: Id

simplify ((Const x1) :*: (Const x2)) = Const(x1*x2)
simplify (Id :*: x1) = Id :*: (simplify x1)
simplify ((Const x1) :*: Id)
 | x1 == 0 = Const 0
 | otherwise = (Const x1):*: Id
 
simplify ((Const x1) :*: Id :+: Id) = (Const (x1 + 1)) :*: Id
simplify ((Const x1) :*: Id :+: (Const x2) :*: Id) = (Const ( x1+x2)) :*: Id
simplify ((Const x1) :*: (x2 :+: x3)) = (simplify (Const x1)) :*: ((simplify x2) :+: (simplify x3))
simplify ((Const x1) :*: (x2 :*: x3)) = (simplify (Const x1)) :*: ((simplify x2) :*: (simplify x3))

simplify (Prim x1) = Prim x1

simplify ((Const x1) :*: (Prim x2)) = (Const x1) :*: (Prim x2)
simplify ((Const x1) :*: (x2 :.: x3)) = (Const x1) :*: ((simplify x2) :.: (simplify x3))
simplify ((Prim x1) :*: x2) = (Prim x1) :*: (simplify x2)

simplify ((x1 :+: x2) :*: x3) = ((simplify x1) :+: (simplify x2)) :*: (simplify x3)
simplify ((x1 :*: x2) :*: x3) = ((simplify x1) :*: (simplify x2)) :*: (simplify x3)
simplify ((x1 :.: x2) :*: x3) = ((simplify x1) :.: (simplify x2)) :*: (simplify x3)

simplify (x1 :.: x2) = (simplify x1) :.: (simplify x2)



module Evaluator
where

infixl 6 :+:
infixl 7 :*:
infixr 1 :?:

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Expr :?: Expr  -- non-deterministic choice
  |  Var String     -- a variable

evalA :: (Applicative f) => Expr -> f Integer
evalA (Lit i)      =  pure i
evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

toss  ::  Expr
toss  =  Lit 0 :?: Lit 1

evalN :: Expr -> [Integer]
evalN (Lit i) = pure i
evalN (e1 :+: e2) = [x + y | x <- evalN e1, y <- evalN e2]
evalN (e1 :*: e2) = [x * y | x <- evalN e1, y <- evalN e2]
evalN (Div e1 e2) = [x `div` y | x <- evalN e1, y <- evalN e2]
evalN (e1 :?: e2) = evalN e1 ++ evalN e2

newtype Environ a = EN { fromEN :: [([Char], Integer)] ->  a }

instance Functor Environ where
-- fmap :: (a -> b) -> Environ a -> Environ b
   fmap f (EN x) = EN $ \env -> f (x env)

instance Applicative Environ where
-- pure a -> Environ a
   pure x = EN (\[(z,y)]->x)
-- (<*>) :: Environ (a -> b) -> Environ a -> Environ b
   EN f <*> EN x  = fmap (f []) (EN x)

evalR :: Expr -> Environ Integer
evalR (Lit x) = pure x
evalR (Var x) = EN (\y -> case (lookup x y) of
  Nothing -> 0
  Just a -> a)
evalR (e1 :+: e2) = EN (\x -> (fromEN(evalR e1) x) + (fromEN (evalR e2) x))
evalR (e1 :*: e2) = EN (\x -> (fromEN(evalR e1) x) * (fromEN (evalR e2) x))
evalR (Div e1 e2) = EN (\x -> (fromEN(evalR e1) x) `div` (fromEN (evalR e2) x))


newtype EnvND a = EnN { fromEnN :: [(String, Integer)] ->  [a] }
instance Functor EnvND where
-- fmap :: (a -> b) -> EnvND a -> EnvND b
   fmap f (EnN x) = error "fmap (Functor EnvND): not yet implemented"

instance Applicative EnvND where
--pure a -> EnvND a
  pure x = EnN (\y -> (x:[]))
--(<*>) :: EnvND (a -> b) -> EnvND a -> EnvND b
  EnN f <*> EnN xs  = error "<*> (Applicative EnvND): not yet implemented"

evalNR :: Expr -> EnvND Integer
evalNR (Lit x) = pure x
evalNR (Var x) = EnN(\y -> case (lookup x y) of
  Nothing -> 0:[]
  Just a -> a:[])
evalNR (e1 :+: e2) = EnN (\z -> [x + y | x <- fromEnN (evalNR e1) z, y <- fromEnN (evalNR e2) z])
evalNR (e1 :*: e2) = EnN (\z -> [x * y | x <- fromEnN (evalNR e1) z, y <- fromEnN (evalNR e2) z])
evalNR (Div e1 e2) = EnN (\z -> [x `div` y | x <- fromEnN (evalNR e1) z, y <- fromEnN (evalNR e2) z])
evalNR (e1 :?: e2) = EnN (\y -> fromEnN (evalNR e1) y ++ fromEnN (evalNR e2) y)

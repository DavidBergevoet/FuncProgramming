-- Harm van der Heide s1047460
-- David Bergevoet s1043736

module RWS
where
import Data.Maybe

newtype RWS r w s a = RWS { fromRWS :: r -> s -> (a, s, w) }

instance Functor (RWS r w s) where
-- fmap :: (a -> b) -> RWS r w s a -> RWS r w s b
--      == (a -> b) -> (r -> s -> (a, s, w)) -> (r -> s -> (b, s, w))
   fmap f (RWS rws) = RWS $ \r -> \s -> let (a,s',w) = rws r s in (f a, s', w)

instance (Monoid w) => Applicative (RWS r w s) where
-- pure :: a -> RWS r w s a
   pure a = RWS $ \r -> \s -> (a,s,mempty)

-- (<*>) :: RWS r w s (a -> b) -> RWS r w s a -> RWS r w s b
   RWS f <*> RWS a = RWS $ \r -> \s -> let (x,y,z) = f r s in
      let (x2,y2,z2) = a r s in
          (x x2,y2 ,mappend z z2)

instance (Monoid w) => Monad (RWS r w s) where
   return = pure
-- (<>>=>) :: RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
   RWS a >>= f = RWS $ \r -> \s -> let (x,y,z) = a r s in
      fromRWS (f x) r s

ask :: (Monoid w) => RWS r w s r
ask = RWS $ \r -> \s -> (r, s, mempty)

get :: (Monoid w) => RWS r w s s
get = RWS $ \_ -> \s -> (s, s, mempty)

put :: (Monoid w) => s -> RWS r w s ()
put s = RWS $ \_ -> \_ -> ((), s, mempty)

tell :: w -> RWS r w s ()
tell w = RWS $ \_ -> \s -> ((), s, w)

data Expr
  =  Lit Integer    -- a literal
  |  Expr :+: Expr  -- addition
  |  Expr :*: Expr  -- multiplication
  |  Div Expr Expr  -- integer division
  |  Var String     -- variables

type Env   = [(String,Integer)]
type Log   = [String]
type Count = Int

evalRWS :: Expr -> RWS Env Log Count Integer
evalRWS (Lit i)      =  RWS $ \env -> \log -> (i,1,[])
evalRWS (e1 :+: e2)  =  RWS $ \r -> \s ->let (v, c, l) = fromRWS(evalRWS e1) r s in
                        let (v2,c2,l2) = fromRWS(evalRWS e2) r s in
                        (v+v2,c2+c,l2++l)
evalRWS (e1 :*: e2)  =  RWS $ \r -> \s ->let (v, c, l) = fromRWS(evalRWS e1) r s in
                        let (v2,c2,l2) = fromRWS(evalRWS e2) r s in
                        (v*v2,c2+c,l2++l)
evalRWS (Div e1 e2)  =  RWS $ \r -> \s ->let (v, c, l) = fromRWS(evalRWS e1) r s in
                        let (v2,c2,l2) = fromRWS(evalRWS e2) r s in
                        if v2 == 0 then (v,c2+c,l2++l++["Division by zero"]) else (v `div` v2,c2+c,l2++l)
evalRWS (Var v)      =  RWS $ \env -> \log -> let val = (lookup v env) in
    case val of
      Nothing -> (0, 0, ["Variable '" ++ v ++ "' not found!"])
      Just a -> (a, 0, ["Variable '" ++ v ++ "' Extracted"])

guard :: Integer -> RWS Env Log Count Integer
guard x
  | x == 0 = RWS $ \r -> \s -> (1, 0, ["Division by zero"])
  | otherwise = RWS $ \r -> \s -> (x, 0, ["Toppie"])

p1 = (fromRWS $ evalRWS (Var "a" :+: Lit 1)) [("a", 4711), ("b", 0815)] 0
p2 = (fromRWS $ evalRWS (Var "a" :+: Var "b")) [("a", 4711), ("b", 0815)] 0
p3 = (fromRWS $ evalRWS (Var "a" :+: Var "c")) [("a", 4711), ("b", 0815)] 0
p4 = (fromRWS $ evalRWS (Div (Var "a") (Var "c"))) [("a", 4711), ("c", 0)] 0

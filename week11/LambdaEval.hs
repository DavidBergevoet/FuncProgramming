-- Harm van der Heide s1047460
-- David Bergevoet s1043736

module LambdaEval
where
import Data.Maybe
import Control.Monad
import Debug.Trace

import qualified Data.Map as Map

infixl 9 :@:

data Expr
  =  Lit Integer      -- a literal
  |  Var String       -- a variable
  |  Bin Expr Op Expr -- binary operator
  |  Abs String Expr  -- a lambda expression
  |  Expr :@: Expr    -- an application
  deriving Show

data Op = Plus | Mult
  deriving Show

data Value = IntVal Integer | FunVal Env String Expr
  deriving Show

newtype Environ a = EN { fromEN :: [([Char], Integer)] ->  a }

instance Functor Environ where
-- fmap :: (a -> b) -> Environ a -> Environ b
   fmap f (EN x) = EN $ \env -> f (x env)

instance Applicative Environ where
-- pure a -> Environ a
   pure x = EN (\env-> x)
-- (<*>) :: Environ (a -> b) -> Environ a -> Environ b
   EN f <*> EN x  = EN $ \env -> f env (x env)

instance Monad Environ where
  return = pure
  --(>>=) :: Environ a -> (a -> Environ b) -> Environ b
  EN x >>= f =  EN $ \env  -> (fromEN (f (x env))env)



type Env = [(String,Value)]

applyIntOp :: Op -> Value -> Value -> Value
applyIntOp op (IntVal v1) (IntVal v2) =
   case op of
      Plus -> IntVal (v1 + v2)
      Mult -> IntVal (v1 * v2)

eval0 :: Expr -> Env -> Value
eval0 (Lit i)        env = IntVal i
eval0 (Var v)        env = fromJust (lookup v env)
eval0 (Bin e1 op e2) env = applyIntOp op (eval0 e1 env) (eval0 e2 env)
eval0 (Abs v b)      env = FunVal env v b
eval0 (ef :@: ea)    env = let FunVal env var body = eval0 ef env
                               arg                 = eval0 ea env
                           in eval0 body ((var,arg):env)

myExpr = Bin (Lit 12) Plus ((Abs "x" (Bin (Var "x") Mult (Lit 2))) :@: Bin (Lit 4) Plus (Lit 2))

eval1 :: Expr -> Env ->Environ Value
eval1 (Lit i)        env = (return $ IntVal i)
eval1 (Var v)        env = return $ fromJust (lookup v env)
eval1 (Bin e1 op e2) env = EN $ \en -> applyIntOp op (fromEN (eval1 e1 env) en) (fromEN (eval1 e2 env) en)
eval1 (Abs v b)      env =  (return $ FunVal env v b)
eval1 (ef :@: ea)    env = do
        val1 <- eval1 ef env
        val2 <- eval1 ea env
        case val1 of
          FunVal env var body -> eval1 body ((var,val2):env)


eval2 :: Expr -> Env ->Environ (Either String Value)
eval2 (Lit i)        env = return $ Right (IntVal i)
eval2 (Var v)        env = return $ case (lookup v env) of
                          Nothing -> Left "Variable not found"
                          Just a -> Right a
eval2 (Bin e1 op e2) env = EN $ \en -> case (fromEN (eval2 e1 env) en) of
                Left a -> Left a
                Right b -> case (fromEN (eval2 e2 env) en) of
                    Left a' -> Left a'
                    Right b' -> Right (applyIntOp op b b')

eval2 (Abs v b)      env =  return $ Right $ FunVal env v b
eval2 (ef :@: ea)    env = do
    val1 <- eval2 ef env
    val2 <- eval2 ea env
    case val1 of
      Left a -> return $ Left a
      Right b -> case b of
          IntVal i -> return $ Left "Left side is an IntVal"
          FunVal env var body ->  (eval2 body ((var,eval0 ea env):env))

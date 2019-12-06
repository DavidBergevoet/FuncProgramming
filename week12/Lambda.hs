-- David Bergevoet    s1043736
-- Harm van der Heide s1047460
module Lambda
where

import Parser
import Control.Applicative

infixl 9 :@

data Lambda var
  = Var var
  | Fun var (Lambda var)
  | Lambda var :@ Lambda var
  deriving (Show)

{-
  There are no spaces in the lambda functions.
  An applicative is variables separated by a space
  Names of variables or lambda's can't be longer than a single char


  "/x->x"     --> Fun "x" (Var "x")
  "/x->/y->z" --> Fun "x"(Fun "y" (Var "z"))
  "a /x->y"   --> Var "a" :@ Fun "x" (Var "y")
  "a"         --> Var "a"
  "a b"       --> Var "a" :@ Var "b"
-}



parseLambdaExpr :: Parser (Lambda Char)
parseLambdaExpr = (do
    char '/'
    x <- letter
    char '-'
    char '>'
    y <- parseLambdaExpr
    return (Fun x (y))
  )
  <|> (do
    x <- letter
    char ' '
    y <- parseLambdaExpr
    return (Var x :@ y)
  )
  <|> (do
    x <- letter
    return (Var x)
  )

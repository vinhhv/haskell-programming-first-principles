module Hutton where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x)                   = x
eval (Add (Lit x) (Lit y))     = x + y
eval (Add (Lit x) y@(Add _ _)) = x + eval y
eval (Add x@(Add _ _) (Lit y)) = eval x + y
eval (Add x@(Add _ _) y@(Add _ _))   = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit x)                       = show x
printExpr (Add (Lit x) (Lit y))         = show x ++ " + " ++ show y
printExpr (Add (Lit x) y@(Add _ _))     = show x ++ " + " ++ printExpr y
printExpr (Add x@(Add _ _) (Lit y))     = printExpr x ++ " + " ++ show y
printExpr (Add x@(Add _ _) y@(Add _ _)) = printExpr x ++ " + " ++ printExpr y

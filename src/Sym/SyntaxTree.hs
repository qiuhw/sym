module Sym.SyntaxTree
    ( Expr(..)
    , evalExpr
    ) where

data Expr = ID String
          | And Expr Expr
          | Or Expr Expr
          | Not Expr Expr

instance Show Expr where
    show (ID  s)     = s
    show (And e1 e2) = parens e1 ++ " interact " ++ parens e2
    show (Or  e1 e2) = parens e1 ++ " or " ++ parens e2
    show (Not e1 e2) = parens e1 ++ " not inside " ++ parens e2

parens (ID s) = s
parens e      = '(' : show e ++ ")"

evalExpr :: Expr -> [String]
evalExpr (ID s) = [s]

evalExpr (And e1 e2) = zipWith (\h t -> h ++ "_" ++ t) head' tail'
  where head  = evalExpr e1
        tail  = evalExpr e2
        head' = concatMap (replicate (length tail)) head
        tail' = concat $ replicate (length head) tail

evalExpr (Or e1 e2) = evalExpr e1 ++ evalExpr e2

evalExpr (Not e1 _) = evalExpr e1

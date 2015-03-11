module L2(tv,
          ar,
          var,
          lam,
          expr,
          typeCheck) where

data Type
     = Var String
     | Arrow Type Type
       deriving (Eq, Ord, Show)

tv = Var
ar = Arrow

data Expr
     = Expr Term Type
       deriving (Eq, Ord, Show)

expr = Expr

data Term
     = Variable String
     | TermAbstraction String Type Term
       deriving (Eq, Ord, Show)

var = Variable
lam = TermAbstraction

typeCheck :: Expr -> Bool
typeCheck (Expr (Variable _) _) = False
typeCheck (Expr (TermAbstraction _ _ _) _) = True

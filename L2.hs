module L2(tv,
          ar,
          var,
          lam,
          expr,
          typeCheck) where

import Data.List as L
import Data.Map as M

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

data Context = Con (Map String Type)

emptyContext = Con M.empty

addVar :: String -> Type -> Context -> Context
addVar name tp (Con m) = Con $ M.insert name tp m

typeInContext :: String -> Context -> Maybe Type
typeInContext name (Con m) = M.lookup name m

typeCheck :: Expr -> Bool
typeCheck e = typeCheckWithContext emptyContext e

typeCheckWithContext :: Context -> Expr -> Bool
typeCheckWithContext c (Expr (Variable name) tp) = case typeInContext name c of
  Just t -> t == tp
  Nothing -> False
typeCheckWithContext c (Expr (TermAbstraction varName varType term) (Arrow l r)) =
  case l == r of
    True -> typeCheckWithContext (addVar varName varType c) (expr term r)
    False -> False
typeCheckWithContext _ _ = False

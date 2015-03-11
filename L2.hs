module L2(tv,
          ar,
          var,
          ap,
          lam,
          expr,
          typeCheck) where

import Data.List as L
import Data.Map as M

data Type
     = Var String
     | Arrow Type Type
       deriving (Eq, Ord)

instance Show Type where
  show (Var name) = name
  show (Arrow l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"

tv = Var
ar = Arrow

data Expr
     = Expr Term Type
       deriving (Eq, Ord)

instance Show Expr where
  show (Expr tm tp) = show tm ++ " : " ++ show tp

expr = Expr

data Term
     = Variable String
     | TermAbstraction String Type Term
     | TermApplication Term Term Type
       deriving (Eq, Ord)

instance Show Term where
  show (Variable name) = name
  show (TermAbstraction name tp tm) =
    "(\\" ++ name ++ " : " ++ show tp ++ " . " ++ show tm ++ ")"
  show (TermApplication l r _) = "(" ++ show l ++ " " ++ show r ++ ")"

var = Variable
lam = TermAbstraction
ap = TermApplication

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
typeCheckWithContext c (Expr (TermApplication l r rt) t) =
  typeCheckWithContext c (expr l (ar rt t)) &&
  typeCheckWithContext c (expr r rt)
typeCheckWithContext _ _ = False

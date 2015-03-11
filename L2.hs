module L2(tv,
          ar,
          pit,
          var,
          ap,
          lam,
          tlam,
          tap,
          expr,
          typeCheck) where

import Data.List as L
import Data.Map as M

data Type
     = Var String
     | Arrow Type Type
     | Star
     | Pi String Type
       deriving (Eq, Ord)

instance Show Type where
  show (Var name) = name
  show (Arrow l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (Pi varName tp) = "(||" ++ varName ++ " : * . " ++ show tp ++ ")"

tv = Var
ar = Arrow
pit = Pi

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
     | TypeAbstraction String Term
     | TypeApplication Term Type
       deriving (Eq, Ord)

instance Show Term where
  show (Variable name) = name
  show (TermAbstraction name tp tm) =
    "(\\" ++ name ++ " : " ++ show tp ++ " . " ++ show tm ++ ")"
  show (TermApplication l r _) = "(" ++ show l ++ " " ++ show r ++ ")"
  show (TypeAbstraction varName t) = "(\\" ++ varName ++ " : * . " ++ show t ++ ")"
  show (TypeApplication l r) = "(" ++ show l ++ " " ++ show r ++ ")"

var = Variable
lam = TermAbstraction
ap = TermApplication
tlam = TypeAbstraction
tap = TypeApplication

data Context = Con (Map String Type)

emptyContext = Con M.empty

addVar :: String -> Type -> Context -> Context
addVar name tp (Con m) = Con $ M.insert name tp m

addTypeVar :: String -> Context -> Context
addTypeVar name c = addVar name Star c

typeInContext :: String -> Context -> Maybe Type
typeInContext name (Con m) = M.lookup name m

data VarSource = VarSource Int

newVarSource = VarSource 0

freshVarName :: VarSource -> (VarSource, String)
freshVarName (VarSource n) = (VarSource (n + 1), "<UNIQUE_VAR>" ++ show n)

typeCheck :: Expr -> Bool
typeCheck e = typeCheckWithContext newVarSource emptyContext e

typeCheckWithContext :: VarSource -> Context -> Expr -> Bool
typeCheckWithContext vs c (Expr (Variable name) tp) = case typeInContext name c of
  Just t -> t == tp
  Nothing -> False
typeCheckWithContext vs c (Expr (TermAbstraction varName varType term) (Arrow l r)) =
  case l == r of
    True -> typeCheckWithContext vs (addVar varName varType c) (expr term r)
    False -> False
typeCheckWithContext vs c (Expr (TermApplication l r rt) t) =
  typeCheckWithContext vs c (expr l (ar rt t)) &&
  typeCheckWithContext vs c (expr r rt)
typeCheckWithContext vs c (Expr (TypeAbstraction absVarName tm) (Pi typeVarName tp)) =
  case absVarName == typeVarName of
    True -> typeCheckWithContext vs (addTypeVar typeVarName c) (expr tm tp)
    False -> False
--typeCheckWithContext vs c (Expr (TypeApplication l r) t) =
--  let (newVS, tvName) = freshVarName vs in
--  typeCheckWithContext newVS c (expr l (typeSub r 
typeCheckWithContext _ _ _ = False

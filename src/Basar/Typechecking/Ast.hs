module Basar.Typechecking.Ast where

import Basar.Parsing.Ast (Ident, Loc)

data Ty
  = TySimple String
  | TyFunc Ty Ty
  deriving (Eq, Ord)

data TyDecl
  = TyDDefun Ident [(Ident, Ty)] [TyStmt] Loc
  deriving (Show, Eq, Ord)

data TyStmt
  = TySExpr TyExpr Loc
  | TySDecl TyDecl Loc
  deriving (Show, Eq, Ord)

data TyExpr
  = TyECall TyExpr TyExpr Ty Loc
  | TyEStr String Ty Loc
  | TyEInt Int Ty Loc
  | TyEFloat Float Ty Loc
  | TyERef Ident Ty Loc
  | TyEGroup TyExpr Ty Loc
  | TyELambda (Ident, Ty) [TyStmt] Ty Loc
  | TyELet [(Ident, TyExpr)] [TyStmt] Ty Loc
  deriving (Show, Eq, Ord)

ty :: TyExpr -> Ty
ty (TyECall _ _ ty _) = ty
ty (TyEStr _ ty _) = ty
ty (TyEInt _ ty _) = ty
ty (TyEFloat _ ty _) = ty
ty (TyERef _ ty _) = ty
ty (TyEGroup _ ty _) = ty
ty (TyELambda _ _ ty _) = ty
ty (TyELet _ _ ty _) = ty

instance Show Ty where
  show (TySimple name) = name
  show (TyFunc argTy returnTy) = concat ["(-> ", show argTy, " ", show returnTy, ")"]

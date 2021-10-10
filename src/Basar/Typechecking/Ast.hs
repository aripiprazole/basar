module Basar.Typechecking.Ast where

import Basar.Parsing.Ast (Ident, Loc)

data Ty
  = SimpleTy String
  | FuncTy Ty Ty
  deriving (Eq, Ord)

data TyDecl
  = TyDefunDecl Ident [(Ident, Ty)] [TyStmt] Loc
  deriving (Show, Eq, Ord)

data TyStmt
  = TyExprStmt TyExpr Loc
  | TyDeclStmt TyDecl Loc
  deriving (Show, Eq, Ord)

data TyExpr
  = TyCallExpr TyExpr TyExpr Ty Loc
  | TyStrExpr String Ty Loc
  | TyIntExpr Int Ty Loc
  | TyFloatExpr Float Ty Loc
  | TyRefExpr Ident Ty Loc
  | TyGroupExpr TyExpr Ty Loc
  | TyLambdaExpr (Ident, Ty) [TyStmt] Ty Loc
  | TyLetExpr [(Ident, TyExpr)] [TyStmt] Ty Loc
  deriving (Show, Eq, Ord)

ty :: TyExpr -> Ty
ty (TyCallExpr _ _ ty _) = ty
ty (TyStrExpr _ ty _) = ty
ty (TyIntExpr _ ty _) = ty
ty (TyFloatExpr _ ty _) = ty
ty (TyRefExpr _ ty _) = ty
ty (TyGroupExpr _ ty _) = ty
ty (TyLambdaExpr _ _ ty _) = ty
ty (TyLetExpr _ _ ty _) = ty

instance Show Ty where
  show (SimpleTy name) = name
  show (FuncTy argTy returnTy) = concat ["(-> ", show argTy, " ", show returnTy, ")"]

module Basar.Parsing.Ast where

data Ident = MkIdent String Loc deriving (Show, Eq, Ord)

data Loc = MkLoc Int Int deriving (Show, Eq, Ord)

newtype Type = MkType String deriving (Show, Eq)

data Decl
  = DDefun Ident [(Ident, Type)] [Stmt] Loc
  deriving (Show, Eq)

data Stmt
  = SExpr Expr Loc
  | SDecl Decl Loc
  deriving (Show, Eq)

data Expr
  = ECall Expr Expr Loc
  | EStr String Loc
  | EInt Int Loc
  | EFloat Float Loc
  | ERef Ident Loc
  | EGroup Expr Loc
  | ELambda (Ident, Type) [Stmt] Loc
  | ELet [(Ident, Expr)] [Stmt] Loc
  deriving (Show, Eq)

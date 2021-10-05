module Basar.Parsing.Ast where

data Ident = MkIdent String Loc deriving (Show)

data Loc = MkLoc Int Int deriving (Show)

newtype Type = MkType String deriving (Show)

data Decl
  = DDefun Ident [(Ident, Type)] [Stmt] Loc
  deriving (Show)

data Stmt
  = SExpr Expr Loc
  | SDefun Ident [(Ident, Type)] [Stmt] Loc
  | SDecl Decl Loc
  deriving (Show)

data Expr
  = ECall Expr Expr Loc
  | EStr String Loc
  | EInt Int Loc
  | EFloat Float Loc
  | ERef Ident Loc
  | EGroup Expr Loc
  | ELambda (Ident, Type) [Stmt] Loc
  | ELet [(Ident, Expr)] [Stmt] Loc
  deriving (Show)

module Basar.Parsing.Ast where

data Range = MkRange deriving (Show)

data Ident = MkIdent String Range deriving (Show)

newtype Type = MkType String deriving (Show)

data Decl
  = DDefun Ident [(Ident, Type)] [Stmt] Range
  deriving (Show)

data Stmt
  = SExpr Expr Range
  | SDefun Ident [(Ident, Type)] [Stmt] Range
  | SDecl Decl Range
  deriving (Show)

data Expr
  = ECall Expr Expr Range
  | EStr String Range
  | EInt Int Range
  | EFloat Float Range
  | ERef Ident Range
  | EGroup Expr Range
  | ELambda (Ident, Type) [Stmt] Range
  | ELet [(Ident, Expr)] [Stmt] Range
  deriving (Show)

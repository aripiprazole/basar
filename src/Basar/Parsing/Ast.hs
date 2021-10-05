module Basar.Parsing.Ast where

data Range = MkRange deriving (Show)

data Ident = MkIdent String Range deriving (Show)

newtype Type = MkType String deriving (Show)

data Expr
  = ECall Expr Expr Range
  | EStr String Range
  | EInt Int Range
  | EFloat Float Range
  | ERef Ident Range
  | EGroup Expr Range
  | ELambda (Ident, Type) [Expr] Range
  | EDefun Ident [(Ident, Type)] [Expr]
  | ELet [(Ident, Expr)] [Expr] Range
  deriving (Show)

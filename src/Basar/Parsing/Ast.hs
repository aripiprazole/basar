module Basar.Parsing.Ast where

data Ident = MkIdent Loc String deriving (Show)

data Loc = MkLoc Int Int deriving (Show)

newtype Type = MkType String deriving (Show)

data Decl
  = DDefun Loc Ident [(Ident, Type)] [Stmt]
  deriving (Show)

data Stmt
  = SExpr Loc Expr
  | SDefun Loc Ident [(Ident, Type)] [Stmt]
  | SDecl Loc Decl
  deriving (Show)

data Expr
  = ECall Loc Expr Expr
  | EStr Loc String
  | EInt Loc Int
  | EFloat Loc Float
  | ERef Loc Ident
  | EGroup Loc Expr
  | ELambda Loc (Ident, Type) [Stmt]
  | ELet Loc [(Ident, Expr)] [Stmt]
  deriving (Show)

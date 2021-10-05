module Basar.Parsing.Ast where

data Range = MkRange deriving (Show)

data Ident = MkIdent Range String deriving (Show)

newtype Type = MkType String deriving (Show)

data Decl
  = DDefun Range Ident [(Ident, Type)] [Stmt]
  deriving (Show)

data Stmt
  = SExpr Range Expr
  | SDefun Range Ident [(Ident, Type)] [Stmt]
  | SDecl Range Decl
  deriving (Show)

data Expr
  = ECall Range Expr Expr
  | EStr Range String
  | EInt Range Int
  | EFloat Range Float
  | ERef Range Ident
  | EGroup Range Expr
  | ELambda Range (Ident, Type) [Stmt]
  | ELet Range [(Ident, Expr)] [Stmt]
  deriving (Show)

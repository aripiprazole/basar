module Basar.Parsing.Ast where

data Ident = Ident String Loc deriving (Show, Eq, Ord)

data Loc = Loc Int Int | UndefinedLoc deriving (Show, Eq, Ord)

newtype Type = Type String deriving (Show, Eq)

data Decl
  = DefunDecl Ident [(Ident, Type)] [Stmt] Loc
  deriving (Show, Eq)

data Stmt
  = ExprStmt Expr Loc
  | DeclStmt Decl Loc
  deriving (Show, Eq)

data Expr
  = CallExpr Expr Expr Loc
  | StrExpr String Loc
  | IntExpr Int Loc
  | FloatExpr Float Loc
  | RefExpr Ident Loc
  | GroupExpr Expr Loc
  | LambdaExpr (Ident, Type) [Stmt] Loc
  | LetExpr [(Ident, Expr)] [Stmt] Loc
  deriving (Show, Eq)

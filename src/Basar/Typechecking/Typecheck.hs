module Basar.Typechecking.Typecheck (typecheck, InferError (MkInferError)) where

import Basar.Parsing.Ast (Decl (..), Expr (..), Ident (..), Loc (..), Stmt (..), Type)
import Basar.Typechecking.Ast (Ty (..), TyDecl (..), TyExpr (..), TyStmt (..), ty)
import Basar.Typechecking.TypeEnv (TypeEnv (..))
import qualified Basar.Typechecking.TypeEnv as T
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Unique (Unique, newUnique)

type Infer a = ExceptT InferError (State T.TypeEnv) a

data InferError = MkInferError Loc String

instance Show InferError where
  show (MkInferError (Loc x y) message) = concat [message, " at ", show x, ":", show y]
  show (MkInferError UndefinedLoc message) = message ++ " at (compile-time defined)"

typecheck :: [Decl] -> Either InferError [TyDecl]
typecheck prog = evalState (runExceptT $ traverse resolveDecl prog) T.defaultEnv

resolveDecl :: Decl -> Infer TyDecl
resolveDecl (DefunDecl name parameters body loc) = do
  env <- lift get

  lift $ put env {enclosing = Just env}

  parameters' <- traverse defineParameter parameters
  body' <- traverse resolveStmt body

  lift $ put env

  return $ TyDefunDecl name parameters' body' loc
  where
    defineParameter :: (Ident, Type) -> Infer (Ident, Ty)
    defineParameter (name, type') = do
      type'' <- lookupType type'
      defineVariable name type''
      return (name, type'')

resolveStmt :: Stmt -> Infer TyStmt
resolveStmt (ExprStmt expr loc) = do
  tyExpr <- resolveExpr expr

  return $ TyExprStmt tyExpr loc
resolveStmt (DeclStmt decl loc) = do
  tyDecl <- resolveDecl decl

  return $ TyDeclStmt tyDecl loc

resolveExpr :: Expr -> Infer TyExpr
resolveExpr (StrExpr str loc) = return $ TyStrExpr str T.stringTy loc
resolveExpr (IntExpr n loc) = return $ TyIntExpr n T.intTy loc
resolveExpr (FloatExpr n loc) = return $ TyFloatExpr n T.floatTy loc
resolveExpr (CallExpr callee arg loc) = do
  tyCallee <- resolveExpr callee
  tyArgument <- resolveExpr arg

  case ty tyCallee of
    FuncTy _ returnTy -> return $ TyCallExpr tyCallee tyArgument returnTy loc
    _ -> throwE $ MkInferError loc "Trying to call a non-function value"
resolveExpr (GroupExpr expr loc) = do
  tyExpr <- resolveExpr expr

  return $ TyGroupExpr tyExpr (ty tyExpr) loc
resolveExpr (RefExpr name loc) = do
  ty <- lookupVariable name

  return $ TyRefExpr name ty loc
resolveExpr (LambdaExpr (argName, argType) body loc) = do
  argTy <- lookupType argType
  bodyTy <- traverse resolveStmt body

  returnTy <- case last bodyTy of
    TyExprStmt expr _ -> return $ ty expr
    _ -> throwE $ MkInferError loc "The last statement of a codeblock should be a Expression Statement"

  return $ TyLambdaExpr (argName, argTy) bodyTy returnTy loc
resolveExpr (LetExpr variables body loc) = do
  variablesTy <- traverse defineLetVariable variables
  bodyTy <- traverse resolveStmt body

  returnTy <- case last bodyTy of
    TyExprStmt expr _ -> return $ ty expr
    _ -> throwE $ MkInferError loc "The last statement of a codeblock should be a Expression Statement"

  return $ TyLetExpr variablesTy bodyTy returnTy loc
  where
    defineLetVariable :: (Ident, Expr) -> Infer (Ident, TyExpr)
    defineLetVariable (name, expr) = do
      expr' <- resolveExpr expr
      defineVariable name $ ty expr'
      return (name, expr')

lookupType :: Type -> Infer Ty
lookupType type' = do
  env <- lift get

  case T.getType type' env of
    Just t -> return t
    Nothing -> throwE $ MkInferError UndefinedLoc $ concat ["Could not find type of parameter ", show type', " (", show type', ")"]

lookupVariable :: Ident -> Infer Ty
lookupVariable (Ident name loc) = do
  env <- lift get

  case T.getVariable name env of
    Just var -> return var
    Nothing -> throwE $ MkInferError loc $ "Unbound variable " ++ name

defineVariable :: Ident -> Ty -> Infer ()
defineVariable (Ident name loc) ty = do
  env <- lift get
  lift $ put (T.defineVariable name ty env)
  return ()
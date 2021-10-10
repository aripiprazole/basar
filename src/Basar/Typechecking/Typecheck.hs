module Basar.Typechecking.Typecheck where

import Basar.Parsing.Ast (Decl (DDefun), Expr (ECall, EFloat, EGroup, EInt, ELambda, ELet, ERef, EStr), Ident (MkIdent), Loc (Loc, UndefinedLoc), Stmt (SDecl, SExpr), Type)
import Basar.Typechecking.Ast (Ty (TyFunc, TySimple), TyDecl (TyDDefun), TyExpr (TyECall, TyEFloat, TyEGroup, TyEInt, TyELambda, TyELet, TyERef, TyEStr), TyStmt (TySDecl, TySExpr), ty)
import Basar.Typechecking.Env (Env (enclosing))
import qualified Basar.Typechecking.Env as E
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Unique (Unique, newUnique)

type Infer a = ExceptT InferError (State E.Env) a

data InferError = MkInferError Loc String

instance Show InferError where
  show (MkInferError (Loc x y) message) = concat [message, " at ", show x, ":", show y]
  show (MkInferError UndefinedLoc message) = message ++ " at (compile-time defined)"

typecheck :: [Decl] -> Either InferError [TyDecl]
typecheck prog = evalState (runExceptT $ typecheck' prog) E.defaultEnv

typecheck' :: [Decl] -> Infer [TyDecl]
typecheck' = traverse resolveDecl
  where
    resolveDecl :: Decl -> Infer TyDecl
    resolveDecl (DDefun name parameters body loc) = do
      env <- lift get

      lift $ put env {enclosing = Just env}

      parameters' <- traverse defineParameter parameters
      body' <- traverse resolveStmt body

      lift $ put env

      return $ TyDDefun name parameters' body' loc
      where
        defineParameter :: (Ident, Type) -> Infer (Ident, Ty)
        defineParameter (name, type') = do
          type'' <- lookupType type'
          defineVariable name type''
          return (name, type'')

    resolveStmt :: Stmt -> Infer TyStmt
    resolveStmt (SExpr expr loc) = do
      tyExpr <- resolveExpr expr

      return $ TySExpr tyExpr loc
    resolveStmt (SDecl decl loc) = do
      tyDecl <- resolveDecl decl

      return $ TySDecl tyDecl loc

    resolveExpr :: Expr -> Infer TyExpr
    resolveExpr (EStr str loc) = return $ TyEStr str E.stringTy loc
    resolveExpr (EInt n loc) = return $ TyEInt n E.intTy loc
    resolveExpr (EFloat n loc) = return $ TyEFloat n E.floatTy loc
    resolveExpr (ECall callee arg loc) = do
      tyCallee <- resolveExpr callee
      tyArgument <- resolveExpr arg

      case ty tyCallee of
        TyFunc _ returnTy -> return $ TyECall tyCallee tyArgument returnTy loc
        _ -> throwE $ MkInferError loc "Trying to call a non-function value"
    resolveExpr (EGroup expr loc) = do
      tyExpr <- resolveExpr expr

      return $ TyEGroup tyExpr (ty tyExpr) loc
    resolveExpr (ERef name loc) = do
      ty <- lookupVariable name

      return $ TyERef name ty loc
    resolveExpr (ELambda (argName, argType) body loc) = do
      argTy <- lookupType argType
      bodyTy <- traverse resolveStmt body

      returnTy <- case last bodyTy of
        TySExpr expr _ -> return $ ty expr
        _ -> throwE $ MkInferError loc "The last statement of a codeblock should be a Expression Statement"

      return $ TyELambda (argName, argTy) bodyTy returnTy loc
    resolveExpr (ELet variables body loc) = do
      variablesTy <- traverse defineLetVariable variables
      bodyTy <- traverse resolveStmt body

      returnTy <- case last bodyTy of
        TySExpr expr _ -> return $ ty expr
        _ -> throwE $ MkInferError loc "The last statement of a codeblock should be a Expression Statement"

      return $ TyELet variablesTy bodyTy returnTy loc
      where
        defineLetVariable :: (Ident, Expr) -> Infer (Ident, TyExpr)
        defineLetVariable (name, expr) = do
          expr' <- resolveExpr expr
          defineVariable name $ ty expr'
          return (name, expr')

lookupType :: Type -> Infer Ty
lookupType type' = do
  env <- lift get

  case E.getType type' env of
    Just t -> return t
    Nothing -> throwE $ MkInferError UndefinedLoc $ concat ["Could not find type of parameter ", show type', " (", show type', ")"]

lookupVariable :: Ident -> Infer Ty
lookupVariable (MkIdent name loc) = do
  env <- lift get

  case E.getVariable name env of
    Just var -> return var
    Nothing -> throwE $ MkInferError loc $ "Unbound variable " ++ name

defineVariable :: Ident -> Ty -> Infer ()
defineVariable (MkIdent name loc) ty = do
  env <- lift get
  lift $ put (E.defineVariable name ty env)
  return ()
module Basar.Typechecking.Typecheck where

import Basar.Parsing.Ast (Decl (DDefun), Expr (ECall, EFloat, EGroup, EInt, ELambda, ELet, ERef, EStr), Ident (MkIdent), Loc (MkLoc), Stmt (SDecl, SExpr), Type)
import Basar.Typechecking.Ast (Ty (TyFunc, TySimple), TyDecl (TyDDefun), TyExpr (TyECall, TyEFloat, TyEGroup, TyEInt, TyELambda, TyELet, TyERef, TyEStr), TyStmt (TySDecl, TySExpr), ty)
import Basar.Typechecking.Env (Env, evaluateType, floatTy, getType, getVariable, intTy, stringTy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Infer a = Either InferError a

data InferError = MkInferError Loc String

instance Show InferError where
  show (MkInferError (MkLoc x y) message) = concat [message, " at ", show x, ":", show y]

typecheck :: [Decl] -> Env -> Infer [TyDecl]
typecheck decls env = traverse (`resolveDecl` env) decls
  where
    resolveDecl :: Decl -> Env -> Infer TyDecl
    resolveDecl (DDefun name parameters body loc) env = do
      parameters' <- traverse defineVariable parameters
      body' <- traverse (`resolveStmt` env) body

      Right $ TyDDefun name parameters' body' loc
      where
        defineVariable :: (Ident, Type) -> Infer (Ident, Ty)
        defineVariable (name@(MkIdent name' _), type') = do
          type'' <- case evaluateType type' env of
            Just t -> Right t
            Nothing -> Left $ MkInferError loc $ concat ["Could not find type of parameter ", name', " (", show type', ")"]

          return (name, type'')

    resolveStmt :: Stmt -> Env -> Infer TyStmt
    resolveStmt (SExpr expr loc) env = do
      tyExpr <- resolveExpr expr env

      Right $ TySExpr tyExpr loc
    resolveStmt (SDecl decl loc) env = do
      tyDecl <- resolveDecl decl env

      Right $ TySDecl tyDecl loc

    resolveExpr :: Expr -> Env -> Infer TyExpr
    resolveExpr (EStr str loc) env = Right $ TyEStr str stringTy loc
    resolveExpr (EInt n loc) env = Right $ TyEInt n intTy loc
    resolveExpr (EFloat n loc) env = Right $ TyEFloat n floatTy loc
    resolveExpr (ECall callee arg loc) env = do
      tyCallee <- resolveExpr callee env
      tyArgument <- resolveExpr arg env

      case ty tyCallee of
        TyFunc _ returnTy -> Right $ TyECall tyCallee tyArgument returnTy loc
        _ -> Left $ MkInferError loc "Trying to call a non-function value"
    resolveExpr (EGroup expr loc) env = do
      tyExpr <- resolveExpr expr env

      Right $ TyEGroup tyExpr (ty tyExpr) loc
    resolveExpr (ERef name@(MkIdent name' _) loc) env = do
      ty <- case getVariable name' env of
        Just ty -> Right ty
        Nothing -> Left $ MkInferError loc $ "Unknown variable " ++ name'

      Right $ TyERef name ty loc
    resolveExpr (ELambda (argName@(MkIdent argName' _), argType) body loc) env = do
      argTy <- case evaluateType argType env of
        Just ty -> Right ty
        Nothing -> Left $ MkInferError loc $ "Could not evaluate " ++ argName'

      bodyTy <- traverse (`resolveStmt` env) body

      returnTy <- case last bodyTy of
        TySExpr expr _ -> Right $ ty expr
        _ -> Left $ MkInferError loc "The last statement of a codeblock should be a Expression Statement"

      Right $ TyELambda (argName, argTy) bodyTy returnTy loc
    resolveExpr (ELet variables body loc) env = do
      variablesTy <- traverse defineVariable variables

      bodyTy <- traverse (`resolveStmt` env) body

      returnTy <- case last bodyTy of
        TySExpr expr _ -> Right $ ty expr
        _ -> Left $ MkInferError loc "The last statement of a codeblock should be a Expression Statement"

      Right $ TyELet variablesTy bodyTy returnTy loc
      where
        defineVariable :: (Ident, Expr) -> Infer (Ident, TyExpr)
        defineVariable (name, expr) = do
          expr' <- resolveExpr expr env

          return (name, expr')

{-# LANGUAGE NamedFieldPuns #-}

module Basar.Typechecking.Env where

import Basar.Parsing.Ast (Ident, Type (MkType))
import Basar.Typechecking.Ast (Ty (TyFunc, TySimple), TyDecl)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

data Env = MkEnv
  { variables :: Map String Ty,
    enclosing :: Maybe Env,
    types :: Map String Ty
  }
  deriving (Show)

getType :: Type -> Env -> Maybe Ty
getType (MkType name) MkEnv {types} = types !? name

getVariable :: String -> Env -> Maybe Ty
getVariable name MkEnv {variables} = variables !? name

defineVariable :: String -> Ty -> Env -> Env
defineVariable name ty env@MkEnv {variables} =
  env
    { variables = M.insert name ty variables
    }

stringTy :: Ty
stringTy = TySimple "string"

intTy :: Ty
intTy = TySimple "int"

floatTy :: Ty
floatTy = TySimple "float"

unitTy :: Ty
unitTy = TySimple "unit"

printTy :: Ty
printTy = TyFunc stringTy unitTy

defaultEnv :: Env
defaultEnv =
  MkEnv
    { variables =
        M.fromList
          [ ("print", printTy)
          ],
      types =
        M.fromList
          [ ("unit", unitTy),
            ("float", floatTy),
            ("string", stringTy),
            ("int", intTy)
          ],
      enclosing = Nothing
    }

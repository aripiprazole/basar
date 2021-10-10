{-# LANGUAGE NamedFieldPuns #-}

module Basar.Typechecking.Env where

import Basar.Parsing.Ast (Ident, Type (Type))
import Basar.Typechecking.Ast (Ty (..), TyDecl)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

data Env = MkEnv
  { variables :: Map String Ty,
    enclosing :: Maybe Env,
    types :: Map String Ty
  }
  deriving (Show)

getType :: Type -> Env -> Maybe Ty
getType (Type name) MkEnv {types} = types !? name

getVariable :: String -> Env -> Maybe Ty
getVariable name MkEnv {variables} = variables !? name

defineVariable :: String -> Ty -> Env -> Env
defineVariable name ty env@MkEnv {variables} =
  env
    { variables = M.insert name ty variables
    }

stringTy :: Ty
stringTy = SimpleTy "string"

intTy :: Ty
intTy = SimpleTy "int"

floatTy :: Ty
floatTy = SimpleTy "float"

unitTy :: Ty
unitTy = SimpleTy "unit"

printTy :: Ty
printTy = FuncTy stringTy unitTy

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

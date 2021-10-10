{-# LANGUAGE NamedFieldPuns #-}

module Basar.Typechecking.TypeEnv
  ( TypeEnv (..),
    getType,
    getVariable,
    defineVariable,
    stringTy,
    intTy,
    floatTy,
    defaultEnv,
  )
where

import Basar.Parsing.Ast (Ident, Type (Type))
import Basar.Typechecking.Ast (Ty (..), TyDecl)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

data TypeEnv = TypeEnv
  { variables :: Map String Ty,
    enclosing :: Maybe TypeEnv,
    types :: Map String Ty
  }
  deriving (Show)

getType :: Type -> TypeEnv -> Maybe Ty
getType (Type name) TypeEnv {types} = types !? name

getVariable :: String -> TypeEnv -> Maybe Ty
getVariable name TypeEnv {variables} = variables !? name

defineVariable :: String -> Ty -> TypeEnv -> TypeEnv
defineVariable name ty env@TypeEnv {variables} =
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

defaultEnv :: TypeEnv
defaultEnv =
  TypeEnv
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

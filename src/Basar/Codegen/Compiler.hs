{-# LANGUAGE RecursiveDo #-}

module Basar.Codegen.Compiler (codegen, compile) where

import Basar.Typechecking.Ast (TyDecl (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, except, throwE)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type (i32, i8, ptr)
import LLVM.Context (withContext)
import LLVM.IRBuilder (named)
import LLVM.IRBuilder.Instruction (call, globalStringPtr, ret)
import LLVM.IRBuilder.Module (buildModule, function)
import LLVM.IRBuilder.Monad ()
import LLVM.Module (File (..), Module, withModuleFromAST, writeLLVMAssemblyToFile)
import System.Process (callCommand, system)

newtype CodegenError = CodegenError String deriving (Show)

compile :: [TyDecl] -> ExceptT String IO ()
compile prog = do
  module' <- case codegen prog of
    Right module' -> pure module'
    Left err -> throwE (show err)

  liftIO $ do
    withContext $ \ctx ->
      withModuleFromAST ctx module' $
        writeLLVMAssemblyToFile (File "code.ll")

    system "clang -c code.ll -o code.o"
    system "clang -o code -v code.o"

  return ()

codegen :: [TyDecl] -> Either CodegenError AST.Module
codegen prog = Right $
  buildModule "code" $ mdo
    puts <- function "puts" [(ptr i8, "msg")] i32 $ \[msg] -> return ()

    function "main" [(i32, "argc"), (ptr (ptr i8), "argv")] i32 $ \[argc, argv] -> mdo
      msg <- globalStringPtr "hello, world" (AST.mkName "msg")
      call puts [(AST.ConstantOperand msg, [])]
      ret $ AST.ConstantOperand (C.Int 32 0)

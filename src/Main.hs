{-# LANGUAGE OverloadedStrings #-}

import Codegen

import qualified LLVM.AST as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as G
import LLVM.Module
import LLVM.Context

import Data.ByteString.Char8 as BS

{-

; ModuleID = 'my cool jit'

define double @main() {
entry:
  %1 = fadd double 1.000000e+01, 2.000000e+01
  ret double %1
}

-}

logic :: LLVM ()
logic =
  define double "main" [] $ do
    let a = Codegen.cons $ C.Float (F.Double 10)
    let b = Codegen.cons $ C.Float (F.Double 20)
    res <- fadd a b
    ret res


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

main :: IO ()
main = toLLVM $ runLLVM AST.defaultModule logic

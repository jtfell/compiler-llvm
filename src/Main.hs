--
-- Main.hs - Top-level program for using the codegen infrastructure
--

{-# LANGUAGE OverloadedStrings #-}

import Codegen

import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as G
import LLVM.Module
import LLVM.Context

import Data.ByteString.Char8 as BS

initArray :: LLVM ()
initArray = do
  -- Declare an external fn
  external int "gc_init" []
  external int "alloc_array" [(int, "length")]

  define int "main" [] $ do
    -- Define a constant
    let a = constant $ C.Int 32 10

    -- Define the external function type (for gc_init)
    let voidFnPtrType = AST.FunctionType int [] False
    let voidPtrType = AST.PointerType voidFnPtrType (A.AddrSpace 0)

    -- Define the external function type (for alloc_array)
    let fnPtrType = AST.FunctionType int [int] False
    let ptrType = AST.PointerType fnPtrType (A.AddrSpace 0)

    -- Call the external fns
    call (externf voidPtrType "gc_init") []
    res <- call (externf ptrType "alloc_array") [a]

    -- Return the result
    ret res
  
  
-- Use LLVM library to generate LLVM IR and print it out
toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

main :: IO ()
main = do
  let ast = runLLVM AST.defaultModule initArray
  toLLVM ast


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

-- An example program to generate code for
logic :: LLVM ()
logic = do

  -- Declare an external fn
  external int "print_int" [(int, "a")]

  define int "main" [] $ do
    -- Define a constant
    let a = constant $ C.Int 32 10

    -- Define the external function type as Ptr (Fn (int -> int))
    let fnPtrType = AST.FunctionType int [int] False
    let ptrType = AST.PointerType fnPtrType (A.AddrSpace 0)

    -- Call the external fn (print the int)
    res <- call (externf ptrType "print_int") [a]

    -- Return the result
    ret res
  
-- Use LLVM library to generate LLVM IR and print it out
toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm

main :: IO ()
main = do
  let ast = runLLVM AST.defaultModule logic
  toLLVM ast


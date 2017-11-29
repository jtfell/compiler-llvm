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
  external double "add_2" [(double, "a")]

  define double "main" [] $ do
    -- Define 2 constants
    let a = Codegen.cons $ C.Float (F.Double 10)
    let b = Codegen.cons $ C.Float (F.Double 20)

    -- Add the 2 constants
    added <- fadd a b

    -- Define the external function type as Ptr (Fn (Dbl -> Dbl))
    let fnPtrType = AST.FunctionType double [double] False
    let ptrType = AST.PointerType fnPtrType (A.AddrSpace 0)

    -- Call the external fn on the result of the add operation
    res <- call (externf ptrType "add_2") [added]

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


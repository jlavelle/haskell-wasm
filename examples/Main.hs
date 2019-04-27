{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (break, sum)

import Language.Wasm.Builder
import Language.Wasm.Structure (ValueType(..))
import Language.Wasm

-- https://github.com/mdn/webassembly-examples/blob/master/js-api-examples/simple.wat
simple :: Module
simple = genMod $ do
  i <- importFunction "imports" "imported_func" () [I32]
  f <- fun () $ i32c 42 *> call i []
  _ <- export "exported_func" f
  pure ()

-- https://github.com/mdn/webassembly-examples/blob/master/js-api-examples/memory.wat
memory' :: Module
memory' = genMod $ do
  _ <- importMemory "js" "mem" 1 Nothing
  f <- fun i32 $ do
    ptr <- param i32
    len <- param i32
    end <- local i32
    sum <- local i32
    end .= ptr `add` (len `mul` i32c 4)
    block () $ do
      break <- label
      loop () $ do
        top <- label
        brIf (ptr `eq` end) break
        sum .= sum `add` (load i32 ptr 0 0)
        ptr .= ptr `add` i32c 4
        br top
    ret sum
  _ <- export "accumulate" f
  pure ()

main :: IO ()
main = print (validate simple)
    *> print (validate memory')

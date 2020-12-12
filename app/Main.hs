{-# LANGUAGE TemplateHaskell, DataKinds #-}

module Main where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.LookupTypes
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Text as T
import Lib


main :: IO ()
main = do
  -- Get the transitive closure of types mentioned in our params and result tables, and print
  let closure = getTransitiveClosure $ S.fromList (
        $(getImageOfGADTUnderTypeFamily ''Method ''MessageParams)
        <> $(getImageOfGADTUnderTypeFamily ''Method ''ResponseResult)
        )
  putStrLn $ formatTSDeclarations $
    [getTypeScriptDeclarations x | TSType x <- S.toList closure]
    & mconcat
    & S.fromList & S.toList -- uniquify

  -- Build TypeScript lookup types for our message tables and print them
  let paramsLookupTableDecl = $(typeFamilyToLookupType ''Method ''MessageParams)
  let resultLookupTableDecl = $(typeFamilyToLookupType ''Method ''ResponseResult)
  putStrLn $ formatTSDeclarations ([paramsLookupTableDecl, resultLookupTableDecl])

  -- Print handler types to demonstrate using the lookup types
  putStrLn ""
  putStrLn "type RequestHandler<T extends keyof MessageParamsLookup> = (key: T, params: MessageParamsLookup<T>): Promise<ResponseResultLookup<T>>};"
  putStrLn "type NotificationHandler<T extends keyof MessageParamsLookup> = (key: T, params: T);"

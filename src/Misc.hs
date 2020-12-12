{-# LANGUAGE ScopedTypeVariables #-}

module Misc where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Proxy


data Empty = Empty deriving (Eq, Ord, Show)
instance A.ToJSON Empty where
  toJSON Empty = A.Null
instance A.FromJSON Empty where
  parseJSON A.Null = pure Empty
  parseJSON _ = mempty
instance TypeScript Empty where
  getTypeScriptType (Proxy :: Proxy Empty) = "null"

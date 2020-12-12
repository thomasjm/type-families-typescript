{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Params where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH
import qualified Data.Text as T


data LoginParams = LoginParams {
  loginUsername :: T.Text
  , loginPassword :: T.Text
  }
$(deriveJSONAndTypeScript A.defaultOptions ''LoginParams)


data ReportClickParams = ReportClickParams {
  reportClickX :: Int
  , reportClickY :: Int
  }
$(deriveJSONAndTypeScript A.defaultOptions ''ReportClickParams)

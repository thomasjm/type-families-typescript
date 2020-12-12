{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Results where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH
import qualified Data.Text as T


data LoginResult = LoginResult {
  loginResultProfilePicture :: T.Text
  }
$(deriveJSONAndTypeScript A.defaultOptions ''LoginResult)

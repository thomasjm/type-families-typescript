{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH
import qualified Data.Text as T
import Data.Kind as Kind
import GHC.Generics
import Misc


data From = FromServer | FromClient
data MethodType = Notification | Request

data Method (f :: From) (t :: MethodType) where
  Authenticate :: Method 'FromClient 'Request
  Render :: Method 'FromClient 'Request

instance A.ToJSON (Method f t) where
  toJSON Authenticate = A.String "authenticate"
  toJSON Render = A.String "render"

data SMethod (m :: Method f t) where
  SRender :: SMethod 'Render
  SAuthenticate :: SMethod 'Authenticate

data SomeMethod where
  SomeMethod :: forall m. SMethod m -> SomeMethod

instance A.FromJSON SomeMethod where
  parseJSON = undefined

deriveTypeScript A.defaultOptions ''Method

type family MessageParams (m :: Method f t) :: Kind.Type where
  MessageParams 'Render = RenderParams
  MessageParams 'Authenticate = Empty

type family ResponseResult (m :: Method f Request) :: Kind.Type where
  ResponseResult 'Render = Int
  ResponseResult 'Authenticate = Empty

class Foo a where
  getString :: a -> String

data RequestMessage (m :: Method f 'Request) =
  RequestMessage {
    _id :: T.Text
    , _method :: SMethod m
    , _params :: MessageParams m
    } deriving Generic

data NotificationMessage (m :: Method f 'Notification) =
  NotificationMessage {
    _method  :: SMethod m
    , _params  :: MessageParams m
    } deriving Generic

data RenderParams = RenderParams {
  renderDocument :: T.Text
  , renderCode :: T.Text
  }
$(deriveJSONAndTypeScript A.defaultOptions ''RenderParams)

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
import Params
import Results


-- * Method

data From = FromServer | FromClient
data MethodType = Notification | Request

data Method (f :: From) (t :: MethodType) where
  Login :: Method 'FromClient 'Request
  ReportClick :: Method 'FromClient 'Request

-- * Message types

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

data ResponseMessage (m :: Method f 'Request) =
  ResponseMessage
    { _id :: Maybe T.Text
    , _result :: Either String (ResponseResult m)
    } deriving Generic

-- * Type families

type family MessageParams (m :: Method f t) :: Kind.Type where
  MessageParams 'Login = LoginParams
  MessageParams 'ReportClick = ReportClickParams

type family ResponseResult (m :: Method f 'Request) :: Kind.Type where
  ResponseResult 'Login = LoginResult


instance A.ToJSON (Method f t) where
  toJSON ReportClick = A.String "authenticate"
  toJSON Login = A.String "login"

data SMethod (m :: Method f t) where
  SLogin :: SMethod 'Login
  SReportClick :: SMethod 'ReportClick

data SomeMethod where
  SomeMethod :: forall m. SMethod m -> SomeMethod

instance A.FromJSON SomeMethod where
  parseJSON = undefined

deriveTypeScript A.defaultOptions ''Method

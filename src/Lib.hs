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
import Control.Applicative
import Data.Aeson.TypeScript.TH
import qualified Data.Text as T
import Data.Kind as Kind
import Data.Void
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
  ResponseResult _ = Void

-- * Helper data

data SMethod (m :: Method f t) where
  SLogin :: SMethod 'Login
  SReportClick :: SMethod 'ReportClick

data SomeMethod where
  SomeMethod :: forall m. SMethod m -> SomeMethod

data SomeClientMethod = forall t (m :: Method FromClient t). SomeClientMethod (SMethod m)
data SomeServerMethod = forall t (m :: Method FromServer t). SomeServerMethod (SMethod m)

-- * Instances

instance A.ToJSON (Method f t) where
  toJSON Login = A.String "login"
  toJSON ReportClick = A.String "report_click"

instance A.FromJSON SomeClientMethod where
  parseJSON (A.String "login") = pure $ SomeClientMethod SLogin
  parseJSON (A.String "report_click") = pure $ SomeClientMethod SReportClick
  parseJSON _ = mempty

instance A.FromJSON SomeServerMethod where
  parseJSON _ = mempty

instance A.FromJSON SomeMethod where
  parseJSON v = client <|> server
    where
      client = do
        SomeClientMethod m <- A.parseJSON v
        pure $ SomeMethod m
      server = do
        SomeServerMethod m <- A.parseJSON v
        pure $ SomeMethod m

deriveTypeScript A.defaultOptions ''Method

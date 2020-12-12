# type-families-typescript

This is a demonstration of how to map a certain cool API design pattern from Haskell to a TypeScript client library.

The pattern comes from the [lsp-types](https://github.com/alanz/lsp/tree/master/lsp-types), which helps power [haskell-language-server](https://github.com/haskell/haskell-language-server/).

Suppose you want to write a service that deals with two kinds of messages: "requests" and "notifications." Requests have an ID attached and require a response containing the same ID, whereas notifications don't require a response. Messages can be sent from either the client or server.

Let's suppose our API supports two client-to-server messages: one a request called `Login` and one a notification called `ReportClick`. We write out our data types like this:

``` haskell
data From = FromServer | FromClient
data MethodType = Notification | Request

data Method (f :: From) (t :: MethodType) where
  Login :: Method 'FromClient 'Request
  ReportClick :: Method 'FromClient 'Notification
```

Here we're using `DataKinds` and `KindSignatures` to tag the different constructors with information about where they come from and what type of message they are.

Now let's write our generic message constructors:

``` haskell
-- | A request
data RequestMessage (m :: Method f 'Request) =
  RequestMessage {
    _id :: T.Text
    , _method :: SMethod m
    , _params :: MessageParams m
    } deriving Generic

-- | A notification
data NotificationMessage (m :: Method f 'Notification) =
  NotificationMessage {
    _method  :: SMethod m
    , _params  :: MessageParams m
    } deriving Generic

-- | A response to a request
data ResponseMessage (m :: Method f 'Request) =
  ResponseMessage
    { _id :: Maybe T.Text
    , _result :: Either String (ResponseResult m)
    } deriving Generic
```

Notice how these message constructors define their `params` and `result` in terms of a type family call. Let's write those type families now:

``` haskell
type family MessageParams (m :: Method f t) :: Kind.Type where
  MessageParams 'Login = LoginParams
  MessageParams 'ReportClick = ReportClickParams

type family ResponseResult (m :: Method f 'Request) :: Kind.Type where
  ResponseResult 'Login = LoginResult
```

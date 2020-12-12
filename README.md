# type-families-typescript

This is a demonstration of how to map a certain cool API design pattern from Haskell to a TypeScript client library.

## The Setup

Suppose you want to write a service that deals with two kinds of messages: "requests" and "notifications." Requests have an ID attached and require a response containing the same ID, whereas notifications don't require a response. Messages can be sent from either the client or server.

This is a nice general framework for a service and is how the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) is designed.

## The Haskell pattern

What's the best way to represent this service in Haskell types? The following pattern comes from the [lsp-types](https://github.com/alanz/lsp/tree/master/lsp-types) library, which helps power [haskell-language-server](https://github.com/haskell/haskell-language-server/).

Let's suppose we want our API to support two client-to-server messages: one a request called `Login` and one a notification called `ReportClick`. We write out our data types like this:

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

As you can see, a `RequestMessage` has an `id` while a `NotificationMessage` does not. A `ResponseMessage` contains a `result`, which an be either a failure or a successful value. Each message is parameterized by a version of `Method`.

Notice how these message constructors define their `params` and `result` in terms of a type family call. Let's write those type families now:

``` haskell
type family MessageParams (m :: Method f t) :: Kind.Type where
  MessageParams 'Login = LoginParams
  MessageParams 'ReportClick = ReportClickParams

type family ResponseResult (m :: Method f 'Request) :: Kind.Type where
  ResponseResult 'Login = LoginResult
```

Once you add some boilerplate `ToJSON/FromJSON/Eq/Ord` instances to this you have a working set of types for defining a server. I won't go in detail about why this is great, but you can look at `haskell-language-server` to see how this setup adds a lot of type safety to your server, helping make sure you return the right response type to each message, etc.

## Mapping it to TypeScript

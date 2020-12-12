# type-families-typescript

This is a demonstration of how to map a certain cool API design pattern from Haskell to a TypeScript client library.

The pattern comes from the [lsp-types](https://github.com/alanz/lsp/tree/master/lsp-types), which helps power [haskell-language-server](https://github.com/haskell/haskell-language-server/).

Suppose you want to write a service that deals with two kinds of messages: "requests" and "notifications". Requests have an ID attached and require a response containing the same ID, whereas notifications don't require a response. Messages can be sent from either the client or server.

Let's suppose our API supports two client-to-server messages: one a request called `Login` and one a notification called `ReportClick`. We write out our data types like this:

``` haskell
data From = FromServer | FromClient
data MethodType = Notification | Request

data Method (f :: From) (t :: MethodType) where
  Login :: Method 'FromClient 'Request
  ReportClick :: Method 'FromClient 'Notification
```

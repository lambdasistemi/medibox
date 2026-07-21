module FFI.WebSocket
  ( WebSocketConnection
  , connect
  , send
  , wsUrl
  ) where

import Prelude

import Effect (Effect)

foreign import data WebSocketConnection :: Type

foreign import connect
  :: String
  -> (String -> Effect Unit)
  -> Effect Unit
  -> Effect Unit
  -> Effect WebSocketConnection

foreign import send :: WebSocketConnection -> String -> Effect Unit

-- | `ws://localhost:<port>/ws`, where `<port>` is taken from the page's
-- | `?wsport=` query parameter if present, else 8080.
foreign import wsUrl :: Effect String

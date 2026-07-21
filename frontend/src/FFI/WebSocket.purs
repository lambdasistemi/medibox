module FFI.WebSocket
  ( WebSocketConnection
  , connect
  , send
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

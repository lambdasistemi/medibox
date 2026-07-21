module FFI.Drag
  ( trackVerticalDrag
  ) where

import Prelude

import Effect (Effect)

foreign import trackVerticalDrag
  :: Int
  -> (Int -> Effect Unit)
  -> Effect Unit
  -> Effect (Effect Unit)

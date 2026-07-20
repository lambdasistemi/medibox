{- | HTTP + WebSocket server. A single @/ws@ endpoint: on connect the
client gets a full 'Medibox.Protocol.Snapshot', after that it's
pure push in both directions.
-}
module Medibox.Server (runServer) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, readTChan)
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.Aeson (decode, encode)
import Data.Foldable (for_)
import Medibox.Protocol (ServerMsg)
import Medibox.Sync (Sync)
import Medibox.Sync qualified as Sync
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

runServer :: Int -> Sync -> IO ()
runServer port sync =
    run port $ websocketsOr WS.defaultConnectionOptions (wsApp sync) httpApp

httpApp :: Wai.Application
httpApp _req respond =
    respond $
        Wai.responseLBS
            status
            [("Content-Type", "text/plain")]
            "medibox backend: use /ws"
  where
    status = toEnum 404

wsApp :: Sync -> WS.ServerApp
wsApp sync pending = do
    conn <- WS.acceptRequest pending
    outbox <- Sync.subscribe sync
    initial <- Sync.snapshot sync
    WS.sendTextData conn (encode initial)
    let pump = forever $ do
            msg <- atomically $ readTChan outbox
            WS.sendTextData conn (encode (msg :: ServerMsg))
    void $ forkIO $ pump `finally` pure ()
    WS.withPingThread conn 30 (pure ()) $
        forever $ do
            raw <- WS.receiveData conn
            for_ (decode raw) (Sync.handleClientMsg sync)

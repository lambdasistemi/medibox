module Main (main) where

import Data.Maybe (fromMaybe)
import Medibox.Midi (listen, withMidi)
import Medibox.Server (runServer)
import Medibox.Store (openStore)
import Medibox.Sync (newSync, onMidiCC)
import System.Environment (lookupEnv)

{- | The MIDI channel the BCR2000 is configured to use for its
control-change layer. Observed default on this unit is 1;
override with @MEDIBOX_MIDI_CHANNEL@ if yours differs.
-}
defaultMidiChannel :: Int
defaultMidiChannel = 1

-- | Default WebSocket port; override with @MEDIBOX_WS_PORT@.
defaultWsPort :: Int
defaultWsPort = 8080

main :: IO ()
main = do
    dbPath <- fromMaybe "medibox.db" <$> lookupEnv "MEDIBOX_DB"
    wsPort <- maybe defaultWsPort read <$> lookupEnv "MEDIBOX_WS_PORT"
    midiChannel <- maybe defaultMidiChannel read <$> lookupEnv "MEDIBOX_MIDI_CHANNEL"
    store <- openStore dbPath
    withMidi midiChannel $ \midi -> do
        sync <- newSync store midi
        _ <- listen midi (onMidiCC sync)
        runServer wsPort sync

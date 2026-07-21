{- | Wires 'Medibox.Midi', 'Medibox.Store' and connected WebSocket
clients together: the single source of truth for "what track is
active" and the fan-out point for state changes.
-}
module Medibox.Sync (
    Sync,
    newSync,
    onMidiCC,
    subscribe,
    handleClientMsg,
    snapshot,
) where

import Control.Concurrent.STM
import Control.Monad (forM_, void)
import Medibox.Midi (Midi, sendCC)
import Medibox.Protocol
import Medibox.Store (Store)
import Medibox.Store qualified as Store

data Sync = Sync
    { syncStore :: Store
    , syncMidi :: Midi
    , syncCurrentSong :: TVar (Maybe Int)
    , syncCurrentTrack :: TVar (Maybe Int)
    , syncBroadcast :: TChan ServerMsg
    }

newSync :: Store -> Midi -> IO Sync
newSync store midi = do
    initialTrack <- Store.currentTrackId store
    initialSong <- maybe (pure Nothing) (Store.trackSongOf store) initialTrack
    songVar <- newTVarIO initialSong
    trackVar <- newTVarIO initialTrack
    Sync store midi songVar trackVar <$> newBroadcastTChanIO

{- | Subscribe a fresh listener to server-pushed messages (typically
one per connected WebSocket client).
-}
subscribe :: Sync -> IO (TChan ServerMsg)
subscribe sync = atomically $ dupTChan (syncBroadcast sync)

publish :: Sync -> ServerMsg -> IO ()
publish sync msg = atomically $ writeTChan (syncBroadcast sync) msg

{- | Called from the MIDI input thread whenever the BCR2000 sends a
Control Change: persists the value against whatever track is
currently active and tells every connected browser.
-}
onMidiCC :: Sync -> Int -> Int -> IO ()
onMidiCC sync cc v = do
    mtid <- readTVarIO (syncCurrentTrack sync)
    forM_ mtid $ \tid -> Store.setParam (syncStore sync) tid cc v
    publish sync $ ParamUpdate cc v

{- | The full state a freshly connected (or just-switched) client
should see.
-}
snapshot :: Sync -> IO ServerMsg
snapshot sync = do
    let store = syncStore sync
    msid <- readTVarIO (syncCurrentSong sync)
    mtid <- readTVarIO (syncCurrentTrack sync)
    songs <- map fromSong <$> Store.listSongs store
    tracks <- case msid of
        Nothing -> pure []
        Just sid -> map fromTrack <$> Store.listTracks store sid
    params <- case mtid of
        Nothing -> pure []
        Just tid -> map fromParam <$> Store.loadTrackParams store tid
    pure $ Snapshot songs tracks msid mtid params

{- | React to a message coming in from one connected browser.
Returns the messages to broadcast to *every* client (the caller
also owns replying only to the sender when that's cheaper, but for
simplicity everything here is broadcast).
-}
handleClientMsg :: Sync -> ClientMsg -> IO ()
handleClientMsg sync = \case
    SetParam cc v -> do
        mtid <- readTVarIO (syncCurrentTrack sync)
        forM_ mtid $ \tid -> Store.setParam (syncStore sync) tid cc v
        sendCC (syncMidi sync) cc v
        publish sync $ ParamUpdate cc v
    SelectTrack tid -> selectTrack sync tid
    SelectSong sid -> do
        -- Song selection only changes which track list the browser
        -- shows; the active track (and its MIDI push) is chosen by a
        -- subsequent SelectTrack.
        atomically $ writeTVar (syncCurrentSong sync) (Just sid)
        snap <- snapshot sync
        publish sync snap
    CreateSong name -> do
        song <- Store.createSong (syncStore sync) name
        atomically $ writeTVar (syncCurrentSong sync) (Just (Store.songId song))
        snap <- snapshot sync
        publish sync snap
    CreateTrack sid name -> do
        atomically $ writeTVar (syncCurrentSong sync) (Just sid)
        void $ Store.createTrack (syncStore sync) sid name
        snap <- snapshot sync
        publish sync snap
    RenameParam cc name -> do
        mtid <- readTVarIO (syncCurrentTrack sync)
        forM_ mtid $ \tid -> Store.renameParam (syncStore sync) tid cc name
        snap <- snapshot sync
        publish sync snap
    DuplicateSong sid -> do
        newSong <- Store.duplicateSong (syncStore sync) sid
        atomically $ writeTVar (syncCurrentSong sync) (Just (Store.songId newSong))
        newTracks <- Store.listTracks (syncStore sync) (Store.songId newSong)
        case newTracks of
            (t : _) -> selectTrack sync (Store.trackId t)
            [] -> do
                snap <- snapshot sync
                publish sync snap
    DuplicateTrack tid targetSid -> do
        newTrack <- Store.duplicateTrack (syncStore sync) tid targetSid
        atomically $ writeTVar (syncCurrentSong sync) (Just targetSid)
        selectTrack sync (Store.trackId newTrack)

{- | Make the given track the active one: persists the selection,
pushes every one of its stored values out over MIDI, and
broadcasts the resulting snapshot to every client.
-}
selectTrack :: Sync -> Int -> IO ()
selectTrack sync tid = do
    msid <- Store.trackSongOf (syncStore sync) tid
    atomically $ do
        forM_ msid $ writeTVar (syncCurrentSong sync) . Just
        writeTVar (syncCurrentTrack sync) (Just tid)
    Store.setCurrentTrackId (syncStore sync) tid
    params <- Store.loadTrackParams (syncStore sync) tid
    forM_ params $ \p -> sendCC (syncMidi sync) (Store.paramCC p) (Store.paramValue p)
    snap <- snapshot sync
    publish sync snap

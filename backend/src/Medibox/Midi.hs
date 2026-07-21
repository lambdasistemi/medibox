{- | ALSA sequencer plumbing for the BCR2000: one input port that
reports incoming Control Change events, one output port that
broadcasts Control Change events to whatever is subscribed to it
(typically the BCR2000 itself, connected via @aconnect@).

The connection/exception-handling shape follows the old
@legacy\/Common.hs@ and @legacy\/TestAlsa.hs@ patterns.
-}
module Medibox.Midi (
    Midi,
    withMidi,
    listen,
    sendCC,
) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (SomeException)
import Control.Exception qualified as Exception
import Control.Monad (forever, void, when)
import Sound.ALSA.Exception qualified as AlsaExc
import Sound.ALSA.Sequencer qualified as SndSeq
import Sound.ALSA.Sequencer.Address qualified as Addr
import Sound.ALSA.Sequencer.Client qualified as Client
import Sound.ALSA.Sequencer.Connect qualified as Connect
import Sound.ALSA.Sequencer.Event qualified as Event
import Sound.ALSA.Sequencer.Port qualified as Port

{- | Either a running ALSA sequencer client with one CC-in / CC-out
port, or a stand-in used when no ALSA sequencer is available on
this machine (e.g. CI runners with no MIDI hardware) -- the rest
of the app runs the same either way, MIDI just becomes a no-op.
-}
data Midi
    = RealMidi
        { midiSeq :: SndSeq.T SndSeq.DuplexMode
        , midiClientId :: Client.T
        , midiPort :: Port.T
        , midiChannel :: Int
        }
    | NoMidi

{- | Open the ALSA client and run the given action with a handle
usable for 'sendCC' and 'listen'. The client and its ports are
torn down when the action returns. If no ALSA sequencer is available
at all, logs a warning and runs the action with a 'Midi' handle that
silently no-ops instead of failing to start.
-}
withMidi :: Int -> (Midi -> IO a) -> IO a
withMidi channel action =
    catchAny (openReal channel action) $ \e -> do
        putStrLn $
            "midi_unavailable: " ++ show e ++ " -- running without MIDI I/O"
        action NoMidi
  where
    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = Exception.catch

openReal :: Int -> (Midi -> IO a) -> IO a
openReal channel action =
    SndSeq.withDefault SndSeq.Block $ \h -> do
        Client.setName h "medibox"
        cid <- Client.getId h
        Port.withSimple
            h
            "control"
            ( Port.caps
                [ Port.capRead
                , Port.capWrite
                , Port.capSubsRead
                , Port.capSubsWrite
                ]
            )
            Port.typeMidiGeneric
            $ \p -> action (RealMidi h cid p channel)

{- | Spawn a reader thread that calls @onCC cc value@ for every
incoming Control Change on the handle's channel. A no-op when MIDI
is unavailable.
-}
listen :: Midi -> (Int -> Int -> IO ()) -> IO (Maybe ThreadId)
listen NoMidi _ = pure Nothing
listen midi@RealMidi{} onCC = Just <$> forkIO (reportLoop midi onCC)

reportLoop :: Midi -> (Int -> Int -> IO ()) -> IO ()
reportLoop NoMidi _ = pure ()
reportLoop RealMidi{midiSeq = h, midiChannel = channel} onCC =
    (`AlsaExc.catch` \e -> putStrLn $ "midi_exception: " ++ AlsaExc.show e) $
        forever $ do
            ev <- Event.input h
            case Event.body ev of
                Event.CtrlEv
                    Event.Controller
                    ( Event.Ctrl
                            (Event.Channel (fromIntegral -> cha))
                            (Event.Parameter (fromIntegral -> cc))
                            (Event.Value (fromIntegral -> v))
                        ) ->
                        when (cha == channel) $ onCC cc v
                _ -> pure ()

{- | Send a Control Change to whatever is subscribed to our output
port (the BCR2000, once connected via @aconnect medibox:0
<device>@). A no-op when MIDI is unavailable.
-}
sendCC :: Midi -> Int -> Int -> IO ()
sendCC NoMidi _ _ = pure ()
sendCC RealMidi{midiSeq = h, midiClientId = cid, midiPort = p, midiChannel = channel} cc v =
    (`AlsaExc.catch` \e -> putStrLn $ "midi_exception: " ++ AlsaExc.show e) $
        void $
            Event.outputDirect h $
                Event.forConnection (Connect.toSubscribers (Addr.Cons cid p)) $
                    Event.CtrlEv
                        Event.Controller
                        ( Event.Ctrl
                            (Event.Channel $ fromIntegral channel)
                            (Event.Parameter $ fromIntegral cc)
                            (Event.Value $ fromIntegral v)
                        )

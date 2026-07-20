-- | ALSA sequencer plumbing for the BCR2000: one input port that
-- reports incoming Control Change events, one output port that
-- broadcasts Control Change events to whatever is subscribed to it
-- (typically the BCR2000 itself, connected via @aconnect@).
--
-- The connection/exception-handling shape follows the old
-- @legacy\/Common.hs@ and @legacy\/TestAlsa.hs@ patterns.
module Medibox.Midi
    ( Midi
    , withMidi
    , listen
    , sendCC
    ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever, void, when)
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Port as Port

-- | A running ALSA sequencer client with one CC-in / CC-out port.
data Midi = Midi
    { midiSeq :: SndSeq.T SndSeq.DuplexMode
    , midiClientId :: Client.T
    , midiPort :: Port.T
    , midiChannel :: Int
    }

-- | Open the ALSA client and run the given action with a handle
-- usable for 'sendCC' and 'listen'. The client and its ports are
-- torn down when the action returns.
withMidi :: Int -> (Midi -> IO a) -> IO a
withMidi channel action =
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
            $ \p -> action (Midi h cid p channel)

-- | Spawn a reader thread that calls @onCC cc value@ for every
-- incoming Control Change on the handle's channel.
listen :: Midi -> (Int -> Int -> IO ()) -> IO ThreadId
listen midi onCC = forkIO $ reportLoop midi onCC

reportLoop :: Midi -> (Int -> Int -> IO ()) -> IO ()
reportLoop Midi{midiSeq = h, midiChannel = channel} onCC =
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

-- | Send a Control Change to whatever is subscribed to our output
-- port (the BCR2000, once connected via @aconnect medibox:0
-- <device>@).
sendCC :: Midi -> Int -> Int -> IO ()
sendCC Midi{midiSeq = h, midiClientId = cid, midiPort = p, midiChannel = channel} cc v =
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

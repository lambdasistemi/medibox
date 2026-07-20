{- |
Play two precisely timed beats simultaneously
where the speed can be controlled by MIDI controllers.

Whenever the speed is changed we have to cancel
the events that are already scheduled.
So we use this example to demonstrate removing output events.
-}

{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module TestAlsa where

import Common (handleExceptionCont, )

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event.RemoveMonad as Remove
import Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq

import qualified System.Exit as Exit
import qualified System.IO as IO
import System.Environment (getArgs, )

import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (mplus,forM_,forever)
import Data.Maybe.HT (toMaybe, )
import Sound.OSC
import Board
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Map as M
import Data.List
import Control.Lens


sequp :: TVar (Board NE) -> IO () 
sequp board = do 
  handleExceptionCont $ do
    h <- ContT $ SndSeq.withDefault SndSeq.Block
    liftIO $ Client.setName h "Haskell-Beat"
    (public :: Port.T)<-
       ContT $ Port.withSimple h "inout"
          (Port.caps [Port.capRead, Port.capSubsRead,
                      Port.capWrite, Port.capSubsWrite])
          (Port.types [Port.typeMidiGeneric, Port.typeApplication])
    -- a clever way to wait until last event has been sent
    private <-
       ContT $ Port.withSimple h "private"
          (Port.caps [Port.capRead, Port.capWrite])
          (Port.types [Port.typeMidiGeneric])
    -- the time accurate queue from alsa seq
    q <- ContT $ Queue.with h
    liftIO $ mainIO board h public q


mainIO :: TVar (Board NE) -> SndSeq.T SndSeq.DuplexMode -> Port.T -> Queue.T -> IO ()
mainIO tt h public  q = do
  -- initialize for using time 
  PortInfo.modify h public $ do
     PortInfo.setTimestamping True
     PortInfo.setTimestampReal True
     PortInfo.setTimestampQueue q

  c <- Client.getId h

  let mkEv t e =
         (Event.simple (Addr.Cons c public) e) {
             Event.queue = q,
             Event.time = Time.consAbs . Time.Real . RealTime.fromDouble $ t
          }

  let  play t c onoff p v =
         (Event.output h $ mkEv t $ Event.NoteEv onoff $
          Event.simpleNote (Event.Channel $ fromIntegral c) p v)

  Queue.control h q Event.QueueStart Nothing

  let schedule t s = do
         ons <- sort <$> pickBoard s <$> readTVarIO tt
         let  k (On c p v) = play t c Event.NoteOn (Event.Pitch $ fromIntegral p) (Event.Velocity $ fromIntegral v)
              k (Off c p) = play t c Event.NoteOff (Event.Pitch $ fromIntegral p) (Event.Velocity $ fromIntegral 0)
         forM_ ons k
         _ <- Event.drainOutput h
         return ()

  -- main loop
  t0 <- Sound.OSC.time
  let   bar = 8 * beat
        beat = 120 / bpm
        bpm = 125
        subd = 64
        quant = bar * inputq
        inputq = 1 / fromIntegral subd
  let go n  = do
         let  t = quant * fromIntegral n
              l = n `mod` subd
              hr = fromIntegral l * inputq
              er = hr + inputq
         sleepThreadUntil $ t + t0 
         schedule (t + quant) (Span hr er)
         go (n + 1) 
  go 0 

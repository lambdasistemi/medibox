{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
-- module MidiComm where


import Prelude hiding (show)

import Sound.ALSA.Sequencer.Address
import Sound.ALSA.Sequencer.Client
import Sound.ALSA.Sequencer.Port
import Sound.ALSA.Sequencer.Event hiding (time)
import Sound.ALSA.Sequencer.Connect
import Sound.ALSA.Sequencer.Queue
import Sound.ALSA.Sequencer 
import Sound.ALSA.Exception  
import Sound.OSC

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as M
import Control.Lens.TH

import Project 

data Parser = NonReg99 Int | NonReg98 Int Int | NonReg6 Int Int Int | Zot [(Int,Int)]


nrparse :: Parser -> (Int,Int) -> Parser
nrparse (Zot _) (x,y) 
    | x == 99 = NonReg99 y
    | otherwise = Zot [(x,y)]
nrparse (NonReg99 i) (x,y) 
    | x == 98 = NonReg98 i y
    | otherwise = Zot [(99,i),(x,y)]
nrparse (NonReg98 i j) (x,y)
    | x == 6 = NonReg6 i j y
    | otherwise = Zot [(99,i),(98,j), (x,y)] 
nrparse (NonReg6 i j f) (x,y)
    | x == 38 = Zot [(i*127 + j, f*127 + y)]
    | otherwise = Zot [(99,i),(98,j),(38,f),(x,y)] 

loop recha h incha s = do
    ev <-  input h
    case body ev of
        CtrlEv Controller (Ctrl 
            (Channel (fromIntegral -> cha)) 
            (Parameter (fromIntegral -> par)) 
            (Value (fromIntegral -> val))
            ) ->  if cha == recha then 
                      do  
                          let s' = nrparse s (par,val)
                          case s' of 
                              Zot evs -> do
                                  t <- time
                                  atomically . mapM_ ( writeTChan incha . (\(x,y) -> E (Controllo x y) t)) $ evs
                              _     -> return ()
                          loop recha h incha s' 
                  else loop recha h incha s
                  
                        
        NoteEv NoteOn (Note
            (Channel (fromIntegral -> cha)) 
            (Pitch (fromIntegral -> par)) 
            (Velocity (fromIntegral -> val))
            _ _
            ) -> do 
              t <- time
              atomically ( writeTChan incha $ E (NotaOn par val) t)
              loop recha h incha s
        NoteEv NoteOff (Note
                (Channel (fromIntegral -> cha)) 
                (Pitch (fromIntegral -> par)) 
                (Velocity (fromIntegral -> val))
                _ _
                ) -> do 
                  t <- time
                  atomically ( writeTChan incha $ E (NotaOff par val) t)
                  loop recha h incha s 
        _ -> loop recha h incha s

data Point = Controllo Int Int | NotaOn Int Int | NotaOff Int Int | Nota Int Int Time deriving (Show,Read)

makePrisms ''Point

type Er = E Point

filterNotes :: TChan Er -> TChan Er -> IO ThreadId
filterNotes inp out = forkIO $ 
    let   loop ws = do
                      e <- atomically $ readTChan inp
                      case e of
                          E (Controllo p v) _ -> do 
                              atomically $ writeTChan out e
                              loop ws
                          E (NotaOn n v) t -> loop $ M.insert n (v,t) ws
                          E (NotaOff n _) t' -> 
                                case M.lookup n ws of
                                    Just (v,t) -> do
                                        atomically $ writeTChan out (E (Nota n v (t' - t)) t)
                                        loop $ M.delete n ws
                                    Nothing -> loop ws
    in loop M.empty


returning a f = f >> return a
-- | Loop-accept control midi message on a specific channel
midiIn  :: String  -- ^ client name
        -> Int     -- ^ listening midi channel
        -> IO (TChan Er)
midiIn name recha = do
  incha <- newTChanIO 
  outcha <- newTChanIO 
  filterNotes incha outcha
  returning outcha $ 
      forkIO $ (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
            withDefault Block $ \h -> do
                setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ " ctrl_in"
                c <- getId h
                withSimple h "ctrl_in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> do 
                            loop recha h incha $ Zot []
data ChanOut = ChanOut {
  active :: TVar Bool
  , events ::   TChan Er
  }

-- | Loopbroadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> Int
        -> Int  -- ^ number or sinks
        -> IO [ChanOut]
midiOut name recha count = do
    bogus <- newTChanIO 
    ech <- forM [0..7] $ \cn -> do
              ech <- newTChanIO 
              state <- newTVarIO False
              count <- newTVarIO $ M.fromList $ zip [0..127] $ repeat 0
              let onstate f = readTVar state >>= flip when f
              forkIO . forever $ do
                e <- atomically $ readTChan ech
                tn <- time
                case e of   
                            E (Controllo par val) t@((>tn) -> True) -> forkIO $ sleepThreadUntil t >> atomically (onstate $ writeTChan bogus $ CtrlEv Controller (Ctrl 
                                                  (Channel $ fromIntegral recha) 
                                                  (Parameter $ fromIntegral par) 
                                                  (Value $ fromIntegral val)
                                                  )) 
                            E (Nota par 0 _) _ -> forkIO $ return () 
                            E (Nota par val dt) t@((>tn) -> True) -> forkIO $ do 
                                                  sleepThreadUntil t 
                                                  atomically (onstate $ writeTChan bogus $ NoteEv NoteOn (Note
                                                                                                (Channel $ fromIntegral recha) 
                                                                                                (Pitch $ fromIntegral par) 
                                                                                                (Velocity $ fromIntegral val)
                                                                                                (Velocity 0) (Duration 0)
                                                                                                )) 
                                                  atomically (modifyTVar count $ M.adjust (+1) par)
                                                  sleepThreadUntil (t + dt) 
                                                  atomically $ do
                                                      modifyTVar count $ M.adjust (subtract 1) par
                                                      c <- flip (M.!) par <$> readTVar count
                                                      when (c <= 0) $ do
                                                                    onstate $ writeTChan bogus $ NoteEv NoteOff (Note
                                                                                                    (Channel $ fromIntegral recha) 
                                                                                                    (Pitch $ fromIntegral par) 
                                                                                                    (Velocity $ fromIntegral val)
                                                                                                    (Velocity 0) (Duration 0)
                                                                                                    ) 
                                                                    modifyTVar count $ M.insert par 0
                            _ -> forkIO $ return ()
              return $ ChanOut state ech    

    forkIO $ (`catch` \e -> putStrLn $ "midi_exception: " ++ show e) $ do
        withDefault Block $ \h -> do
            setName (h :: Sound.ALSA.Sequencer.T OutputMode) $ name ++ " ctrl_out"
            c <- getId h
            withSimple h "ctrl_out" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                
                o <- atomically $ readTChan bogus
                case o of 
                    CtrlEv Controller (Ctrl 
                        (Channel (fromIntegral -> cha)) 
                        (Parameter (fromIntegral -> par)) 
                        (Value (fromIntegral -> val))
                        ) -> 
                            if val < 128  && par < 128 then     
                                void $ outputDirect h $ forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ o
                            else  do
                              let       (p1,p2) = par `divMod` 127
                                        (v1,v2) = val `divMod` 127
                              forM_ (zip [99,98,6,38] [p1,p2,v1,v2]) $ \(par,val) -> do
                                  let ev =  forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ 
                                        CtrlEv Controller (Ctrl 
                                            (Channel $ fromIntegral cha) 
                                            (Parameter $ fromIntegral par) 
                                            (Value $ fromIntegral val)
                                            )
                                  void $ outputDirect h $ ev
                    o -> void $ outputDirect h $ forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $ o
    return ech
midiInTime  :: String  -- ^ client name
        -> IO ThreadId
midiInTime name = 
      forkIO $ (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
            withDefault Block $ \h -> do
                setName (h :: Sound.ALSA.Sequencer.T InputMode) $ name ++ " ctrl_in"
                c <- getId h
                withSimple h "ctrl_in" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \p -> forever $ input h >>= print

midiOutTime  :: String  -- ^ client name
        -> Time
        -> IO ThreadId
midiOutTime name t = do
      t0 <- time
      let n = fromIntegral . floor $ t0 / t
      forkIO $ (`catch` \e -> putStrLn $ "midi_exception: " ++ show e)  $ do
            withDefault Block $ \h -> do
                setName (h :: Sound.ALSA.Sequencer.T OutputMode) $ name ++ " time_out"
                c <- getId h
                withSimple h "ctrl_in" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> 
                    let loop n = do
                            sleepThreadUntil $ n * t
                            void $ outputDirect h $ forConnection (toSubscribers (Sound.ALSA.Sequencer.Address.Cons c p)) $  QueueEv QueueClock Sound.ALSA.Sequencer.Queue.direct
                            loop $ n + 1
                    in loop 1

main = do
  midiOutTime "timeout" $ 0.125/6
  getLine

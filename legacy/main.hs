{-# LANGUAGE ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception
import Sound.OSC
import Project
import MidiComm
import qualified Data.Map as M
import Control.Lens
import Control.Lens.TH
import Data.List
import Data.Ord
import System.Random

data S = S {
  _width :: Int
  , _base :: Int
  }

makeLenses ''S

data P = P {
  _number :: Int
  , _timep :: Time
  , _pitch :: S
  , _vel :: S 
  }

makeLenses ''P
  
randomS (S w b) = (b + ) <$> randomRIO (0, w -1)

randomNotes :: P -> IO [Er]
randomNotes (P n dt sp sv) = do
    t0 <- time 
    forM [1 .. n] $ \_ -> do
      p <- randomS sp
      v <- randomS sv
      t <- randomRIO (0,dt)
      return (E (NotaOn p v) $ t + t0) 

choice a b = (Left `fmap` a) `orElse` (Right `fmap` b)

nooff (NotaOn _ _) = True
nooff _ = False

onNotaOn f (NotaOn n v) = f (NotaOn n v)
onNotaOn _ x = x


data L = R deriving Read

main = do
  inc <- midiIn "notes" 0 
  out <- midiOut "test" 0 8 
  atomically $ writeTVar (active $ out !! 0) True
  term <- newTChanIO
  

  (les,lm) <- (read <$> readFile "test.dot") `catch` (\(_::IOException) -> return ([],M.empty))

  mappa <- newTVarIO (M.findWithDefault M.empty 0 lm)
  record <- newTVarIO lm
  rnotes <- newTVarIO les
  nnotes <- newTVarIO 8
  spitch <- newTVarIO (S 7 30)
  svel <- newTVarIO (S 100 10)
  inc <- midiIn "controls" 1 
  outc <- midiOut "replica" 1 1
  sleepThread 2
  atomically $ writeTVar (active $ outc !! 0) True
  let replica t m = forM_ [0..127] $ \p -> writeTChan (events $ outc !! 0) $ E (Controllo p $ M.findWithDefault 0 p m) t 
  t <- time
  atomically $ readTVar mappa >>= replica t
  selected <- newTVarIO $  0
  forkIO $ 
      let 
          loop n = do
                  t <- time
                  n' <- atomically $ do
                                  r <- readTChan inc
                                  case r of 
                                    E (Controllo 58 n') _ -> do
                                        m <- readTVar mappa
                                        modifyTVar record $ M.insert n m --record old
                                        m <- M.findWithDefault M.empty n' <$> readTVar record -- get new from record
                                        writeTVar mappa m 
                                        writeTVar (active $ out !! n) False
                                        writeTVar (active $ out !! n') True
                                        writeTVar selected $ n'
                                        replica t m
                                        return n'
                                    E (Controllo 59 n') _ -> do
                                        m <- readTVar mappa
                                        modifyTVar record $ M.insert n' m --record new
                                        writeTVar (active $ out !! n) False
                                        writeTVar (active $ out !! n') True
                                        writeTVar selected n'
                                        return n'
                                    E (Controllo p v) _ -> do
                                        modifyTVar mappa $ M.insert p v
                                        return n  
                                    _ -> return n
                  loop n'
      in loop 0

  exit <- newTVarIO False
  forkIO .forever $ do 
      l <- getLine
      a <- atomically $ case reads l of 
                      [(R,_)] -> do
                        p <- readTVar spitch
                        v <- readTVar svel
                        n <- readTVar nnotes
                        return $ randomNotes (P n (16*0.25) p v) >>= atomically . writeTVar rnotes 
                      _ ->   writeTVar exit True >> return (return ())
      a
  let loop = do
        xs <- atomically $ do 
            xs <- readTVar rnotes
            when (null xs) retry
            return xs
        t <- time
        forM_ [0..1] $ \nm -> atomically $ do  
                                  m <- do
                                            n <- readTVar selected
                                            if nm == n then readTVar mappa else M.findWithDefault M.empty nm <$> readTVar record 
                                  let E _ t0 : _ = xs
                                  let xs' = concat . flip map [0..7] $ \n -> 
                                            let 
                                              fv v = floor . (* fromIntegral v) . (/128) . fromIntegral .M.findWithDefault 0 (24 + n) $ m
                                              s = floor  . (*50) . (/128) . fromIntegral .M.findWithDefault 0 n $ m
                                              ro = (+ 0.5) . (*2) . (/128) . fromIntegral . M.findWithDefault 0 (8 + n) $ m
                                              dt = (* 0.125) . (*16) . (/128) . fromIntegral . M.findWithDefault 0 (16 + n) $ m
                                            in map (over event (onNotaOn $ \(NotaOn n v) -> NotaOn (n + s - 10) $ fv v).  over timet (\tn -> (tn - t0) * ro + t + dt)) xs
                                  mapM_ (writeTChan $ events $ out !! nm)  xs'
        sleepThreadUntil $ t + 16*0.125
        print xs
        c <- readTVarIO exit
        if c then return () else loop
  loop
  xs <- readTVarIO rnotes
  readTVarIO record >>= writeFile "test.dot" . show . ((,) xs) 
    


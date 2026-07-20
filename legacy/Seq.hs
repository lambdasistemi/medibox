
{-# LANGUAGE ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Exception
import Sound.OSC
import qualified Data.Map as M
import Control.Lens
import Control.Lens.TH
import Data.List 
import Data.Ord
import System.Random
import Control.Arrow
import Data.Traversable
import System.Console.Haskeline

import TestAlsa
import Board

type Controls = M.Map Int Int
readValue :: Int -> Double -> Double -> Controls -> Double
readValue i f s = (+s) . (*f) . (/128) . fromIntegral . M.findWithDefault 0 i

data Morph = Morph {
  _shiftNote :: Int 
  , _zoomVolume ::Double
  }deriving (Show,Read)

makeLenses ''Morph


type TrackId = Int

readMorph :: TrackId -> Controls -> Morph
readMorph n m = Morph 
  (floor $ readValue n 50 0 m) 
  (readValue (24 + n) 1 0 m)
        

morph :: Morph -> E N -> E N
morph (Morph dn vo) = over event $ \(N c n v d) -> 
    N c (n + dn) (floor (vo * fromIntegral v)) d

data Displace = Displace {
  _shiftTime :: Time , 
  _expandTime :: Time 
  }deriving (Show,Read)

makeLenses ''Displace

displace :: Displace -> E a -> E a
displace (Displace dt ro) = over timet ((+dt) . (*ro)) where

readDisplace n m =  Displace 
  (readValue (16 + n) 4 0 m) 
  (readValue (8 + n)  2 0 m)
  
data Repeat = Repeat {
  _repeatSubd :: Int,
  _repeatCount :: Int
  } deriving (Show,Read)

repea ::  Repeat  -> Int -> Int -> E a -> E a
repea (Repeat n s) i k  = over timet (+ (fromIntegral k / (fromIntegral s + 1) / (fromIntegral n + 1))) . over timet (+ (fromIntegral i / (fromIntegral n + 1)))

makeLenses ''Repeat

readRepeat n m = Repeat
  (floor (readValue (32 + n) 128 0 m))
  (floor (readValue (40 + n) 128 0 m))

data Track = Track {
  _morpher :: Morph,
  _displacer ::  Displace,
  _repeater :: Repeat , 
  -- _randomness :: R,
  _eventi :: [E N]
  } deriving (Show,Read)

makeLenses ''Track 


dispatch :: Track -> [E N]
dispatch (Track m d r@(Repeat n s) es ) = [0..n] >>= \n -> [0..s] >>= f n where
  l = length es
  es' = take ((l `div` (n + 1) `div` (s + 1)) + 1) es
  f i k = map (repea r i k . displace d . morph m) es'
    

updateTrack ::  Controls -> TrackId -> Track -> Track
updateTrack m i (Track  _ _ _ es ) = Track (readMorph i m ) (readDisplace i m)  (readRepeat i m) es





-- listen to notes and substitute the nearest event
updateNotes :: M.Map Int Int -> [Track] -> Board NE 
updateNotes ctrls  = concatMap convert . concatMap dispatch . zipWith (updateTrack ctrls) [0..]  


data L = LR Int R | P | T deriving Read 
data R = R Int Int Int Int Int Int Time deriving Read 

randomNotes :: R -> Int -> IO [E N]
randomNotes (R s n p0 dp v0 dv dl) c = do
    setStdGen (mkStdGen s)
    forM [1 .. n] $ \_ -> do
          p <- randomRIO (p0,p0 + dp)
          v <- randomRIO (v0,v0 + dv)
          t <-  randomRIO (0,1)
          dl' <- randomRIO (0,dl)
          return $  E (N c p v dl') t



l = LR 0 (R 1 5 48 5 30 80 0.3)

input  tracks exit = forever $ do 
      l <- getInputLine "> "
      liftIO . join . atomically $ case reads <$> l of 
                      Just [(T,_)] -> return $ readTVarIO tracks >>= mapM_ print
                      Just [(LR c r,_)] -> do
                        return $ do 
                                  es <- randomNotes r c
                                  atomically $  
                                          modifyTVar tracks $ M.adjust (set eventi es) c
                                  
                      _ ->   writeTChan exit () >> return (return ())

pg c = [E (N c 50 50 0.8) 0 , E (N c 60 50 0.8)  0.25 ,E (N c 40 50 0.8) 0.65 , E (N c 70 50 0.8) 0.5 , E (N c 90 50 0.8) 0.75] 

main = do
  board <- newTVarIO [] 
  cmap <- newTVarIO M.empty 

  tracks <- newTVarIO $ M.fromList . flip map [0..7] $ \n -> (n,  Track  (Morph 0 100) (Displace 0 1) (Repeat 0 0)  $ pg n)
  u <- newTChanIO 
  forkIO . forever . atomically $ do
      () <- readTChan u
      c <- readTVar cmap
      ts <- readTVar tracks 
      writeTVar board (updateNotes c $ M.elems ts)
      
    
  exit <- newTChanIO
  forkIO $ sequp u board cmap
  forkIO . runInputT defaultSettings $ input tracks exit
  atomically $ readTChan exit

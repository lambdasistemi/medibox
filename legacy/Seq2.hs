
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
import Control.Monad.Random
import Control.Arrow
import Data.Traversable
import System.Console.Haskeline

import TestAlsa
import Board
import Interface

type Controls = M.Map Int Int

readValue :: Int -> Double -> Double -> Controls -> Double
readValue i f s = (+s) . (*f) . (/128) . fromIntegral . M.findWithDefault 0 i

type TrackId = Int
        


data Displace = Displace {
  _shiftTime :: Time , 
  _expandTime :: Time 
  }deriving (Show,Read)

displace :: Displace -> E a -> E a
displace (Displace dt ro) = over timet ((+dt) . (*ro)) where

readDisplace n m =  Displace 
  (readValue (0 + n) 4 0 m) 
  (readValue (8 + n)  2 0 m)
  
data Repeat = Repeat {
  _repeatSubd :: Int,
  _repeatCount :: Int
  } deriving (Show,Read)

repea ::  Repeat  -> Int -> Int -> E a -> E a
repea (Repeat (fromIntegral -> n) (fromIntegral -> s)) (fromIntegral -> i) (fromIntegral -> k)  = over timet (+ (1/(n + 1)*(i +  k / (s + 1))))


readRepeat n m = Repeat
  (floor (readValue (16 + n) 128 0 m))
  (floor (readValue (24 + n) 128 0 m))


data R = R Int Int Int Int Int Int Time deriving (Read, Show)

randomness :: Int -> R -> [E N]
randomness c (R seed n  p dp v dv dt) = flip evalRand (mkStdGen seed) . replicateM n $ do
          p <- getRandomR (p,p + dp)
          v <- getRandomR (v,v + dv)
          t <-  getRandomR(0,1)
          dt <- getRandomR (0,dt)
          return $  E (N c p v dt) t

readRandomness n  m = R
    (floor (readValue (32 + n) 128 0 m))
    (floor (readValue (40 + n) 128 0 m))
    (floor (readValue (48 + n) 128 0 m))
    (floor (readValue (56 + n) 128 0 m))
    (floor (readValue (64 + n) 128 0 m))
    (floor (readValue (72 + n) 128 0 m))
    (readValue (80 + n) 1 0 m)

data Track = Track {
  _displacer ::  Displace,
  _repeater :: Repeat , 
  _randomnesser :: R,
  _sections :: M.Map Int Bool
  } deriving (Show,Read)

makeLenses ''Track 


events :: Int -> Track -> [E N]
events c (Track d r@(Repeat n s) ra se) = [0..n] >>= \i -> [0..s] >>= f i where
  es = randomness c ra
  l = length es
  es' i k = case se M.! (i * (s + 1) + k) of
        True ->  take ((l `div` (n + 1) `div` (s + 1)) + 1) es
        _ -> []
  f i k = map (repea r i k . displace d ) (es' i k)
    

fromControls ::  Channell -> Int -> Track
fromControls  (Channell m cs)  i = Track (readDisplace i m)  (readRepeat i m ) (readRandomness i m) (cs M.! i)

-- listen to notes and substitute the nearest event
boarder :: Channell -> Board N
boarder c =  [0 ..7] >>= (events `ap` (fromControls c)) 


data L =  P | T | Q deriving Read 

main = do
  board <- newTVarIO [] 
  forkIO $ sequp board
  change <- newTChanIO 
  forkIO . forever . atomically $ do
                c <- readTChan change
                writeTVar board $ boarder c >>= convert 
  gui change



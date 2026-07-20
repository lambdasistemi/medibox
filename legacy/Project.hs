{-# LANGUAGE TemplateHaskell #-}

module Project where

import Control.Lens
import Control.Lens.TH
import Sound.OSC

data E a = E {
  _event :: a,
  _timet :: Time
  } deriving (Show,Read)

makeLenses ''E

data Linear = Linear Double Double

project :: Linear ->  Time -> Time
project (Linear a b) t = t * a + b

projectE :: Linear -> E a -> E a
projectE l e = over timet (project l) e

window1 :: Time -> Time -> Time -> Time
window1 d tn t = 
  let
    nn = (d *) . fromIntegral $ floor (tn / d)
    nt = (d *) . fromIntegral $ floor (t / d)
    t' = nn + (t - nt)
  in if t' > tn then t' else t' + d


window :: Time -> Time -> Time 
window d tn = (d *) . (+1) . fromIntegral $ floor (tn / d)


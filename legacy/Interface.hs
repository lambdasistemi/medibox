
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings , TemplateHaskell,  ViewPatterns, Rank2Types#-}
module Interface where

import Graphics.UI.Gtk

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Data.IORef
import Data.Text hiding (zip,null)
import qualified Data.Map as M
import Text.Printf
import Control.Lens (over,view,preview,Prism',_1)
import Control.Lens.TH



data Channell = Channell {
        _controls :: M.Map Int Int,
        _checks :: M.Map Int (M.Map Int Bool)
        } deriving (Show,Read)
makeLenses ''Channell


gui ::   TChan Channell -> IO ()
gui update = do
  updateInternal <- newTChanIO 
  tboard <- newTVarIO $ M.fromList $ zip [0..15] $ repeat (Channell (M.fromList $ zip [0..127] $ repeat 0) (M.fromList $ zip [0..8] $ repeat $ M.fromList $ zip[0..23] $ repeat False)):: IO (TVar (M.Map Int Channell))
  tsel <- newTVarIO 0
  let signal = do
        sel <- readTVar tsel
        c <- flip (M.!) sel <$> readTVar tboard
        writeTChan update c
  initGUI
  window <- windowNew
  -- midi listening

  -- main box   
  mainbox    <- vBoxNew False 1
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
                          containerBorderWidth := 10, containerChild := mainbox]
  -- main buttons
  coms <- hBoxNew False 1
  boxPackStart mainbox coms PackNatural 0
  load <- buttonNewFromStock "Load"
  boxPackStart coms load PackNatural 0
  new <- buttonNewWithLabel ("New" :: Text)
  boxPackStart coms new PackNatural 0
  filename <- entryNew
  boxPackStart coms filename PackNatural 0
  fc <- fileChooserButtonNew ("Select a file"::Text) FileChooserActionOpen
  boxPackStart coms fc PackGrow 0
  save <- buttonNewFromStock "Save"
  boxPackStart coms save PackNatural 0
  quit <- buttonNewFromStock "Quit"
  boxPackStart coms quit PackNatural 0

  selecter <- vBoxNew False 1
  res0 <- radioButtonNewWithLabel ("00"::String)

  forM_ [0..15] $ \n ->  do
      lb <- hBoxNew False 1
      boxPackStart selecter lb PackNatural 0
      res <- case n of 
        0 -> return res0 
        _ -> radioButtonNewWithLabelFromWidget res0 (printf "%02d" n :: String) 
      
      res `on` buttonActivated $ do
        atomically $ do 
            writeTVar tsel $ n
            signal
            writeTChan updateInternal ()
      copy <- buttonNewWithLabel $ ("C"::Text)
      boxPackStart lb copy PackNatural 0
      boxPackStart lb res PackNatural 0
      copy `on` buttonActivated $ do
        atomically $ do
            sel <- readTVar tsel 
            modifyTVar tboard $ \b -> M.insert n (b M.! sel) b 
            signal
            writeTChan updateInternal ()
        buttonClicked res
 
  new `on` buttonActivated $ do
        name <- entryGetText filename
        when (not . null $ name) $ do 
          readTVarIO tboard >>= writeFile name . show
          fileChooserSetFilename fc $ name
          return ()
  {- broken ?
  fc `on` fileActivated $  do 
        Just name <- fileChooserGetFilename fc
        entrySetText filename $ name
  -}
  -- main buttons actions
  quit `on` buttonActivated $ mainQuit
  save `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> readTVarIO tboard >>= writeFile name . show
  load `on` buttonActivated $ do
        mname <- fileChooserGetFilename fc
        case mname of 
                Nothing -> return ()
                Just name -> do 
                        readFile name >>= atomically . writeTVar tboard . read  
                        atomically signal
                        atomically $ writeTChan updateInternal ()
  -- knobs
  -- ad <- adjustmentNew 0 0 400 1 10 400
  -- sw <- scrolledWindowNew Nothing (Just ad)
  controlbox <- hBoxNew False 1
  notes <- vBoxNew False 1
  boxPackStart mainbox notes PackNatural 0
  boxPackStart mainbox controlbox PackNatural 0
  boxPackStart controlbox selecter PackNatural 0
  
  forM_ [0..7::Int] $ \m ->  do
        noteline <- hBoxNew False 1
        boxPackStart notes noteline PackNatural 0
                
        forM_ [0..23] $ \n ->  do
              lb <- checkButtonNewWithLabel (printf "%03d" n :: String)  
              boxPackStart noteline lb PackNatural 0
              lb `on` toggled $ do 
                        x <- toggleButtonGetActive lb
                        atomically $ do
                                sel <- readTVar tsel
                                modifyTVar tboard $ M.adjust (over checks $ M.adjust (M.insert n x) m) sel
                                signal
              update' <- atomically $ dupTChan updateInternal
              forkIO . forever $ do 
                        x <- atomically $ do
                          readTChan update' 
                          sel <- readTVar tsel
                          flip (M.!) n  <$> flip (M.!) m <$> view checks <$> flip (M.!) sel <$> readTVar tboard
                        postGUISync $ toggleButtonSetActive lb x
                        

  fillnobbox <- hBoxNew False 1
  boxPackStart controlbox fillnobbox PackNatural 0
  knoblines <- vBoxNew False 1
  boxPackStart fillnobbox knoblines PackNatural 0
   
  forM_ [0..3] $ \paramv' -> do
     knobboxspace    <- hBoxNew True 1
     widgetSetSizeRequest knobboxspace (-1) 10
     boxPackStart knoblines knobboxspace PackNatural 0
     knobbox    <- hBoxNew True 1
     boxPackStart knoblines knobbox PackNatural 0
     forM_ [0..31] $ \paramv'' -> do
          let paramv= paramv' *32 + paramv''
          let c = paramv `mod` 8
          when (c == 0) $ do
                  hbox    <- vBoxNew False 1
                  boxPackStart knobbox hbox PackNatural 0
                 
          hbox    <- vBoxNew False 1
          widgetSetSizeRequest hbox (-1) 135
          boxPackStart knobbox hbox PackNatural 0

          param <- labelNew (Just $ show paramv)
          widgetSetSizeRequest param 28 15
          frame <- frameNew
          set frame [containerChild:= param]
          boxPackStart hbox frame PackNatural 0


          eb <- eventBoxNew
          memory <- newIORef 0
          ad <- adjustmentNew 0 0 127 1 1 1
          level <- vScaleNewWithRange 0 127 1 -- progressBarNew 
          rangeSetAdjustment level ad
          -- progressBarSetOrientation level ProgressBottomToTop
          set eb [containerChild:= level]
          widgetAddEvents eb [Button1MotionMask]
          boxPackStart hbox eb PackGrow 0
          rangeSetValue level 0 -- progressBarSetFraction level 0
         
          -- track load of new parameter sets or midiin
          update' <- atomically $ dupTChan updateInternal
          forkIO . forever $ do 
                wx <- atomically $ do
                  readTChan update'
                  sel <- readTVar tsel
                  wx <- flip (M.!) paramv  <$> view controls <$> flip (M.!) sel <$> readTVar tboard
                  return wx
                postGUISync $ rangeSetValue level  (fromIntegral wx)  -- progressBarSetFraction level (fromIntegral wx * k)
          on level valueChanged $ do 
              x <- rangeGetValue level -- progressBarGetFraction level 
              atomically $ do 
                  let z = floor x
                  sel <- readTVar tsel
                  modifyTVar tboard $ M.adjust (over controls $ M.insert paramv z) sel
                  signal

  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI

limitedAdd xm d x
        | x + d > xm = xm
        | otherwise = x + d
limitedSubtract xm d x
        | x - d < xm = xm
        | otherwise = x - d

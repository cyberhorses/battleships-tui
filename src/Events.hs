{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Events
  ( appCursor
  , appEvent
  ) where

import Brick
  ( BrickEvent(..)
  , EventM
  , halt
  )

import Game
  ( startHosting
  , startJoining
  , exit
  , waitForClient
  , waitForReady
  , sendReady
  , NetworkEvent(..)
  )

import State
-- import Ships

import qualified Graphics.Vty as V
import Control.Concurrent (forkIO)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.Types as T
import qualified Data.Map as M

import Ships
import Lens.Micro
import Lens.Micro.Mtl

import Control.Monad.IO.Class
import Control.Monad (void, when)


---- Event handler -------------------------------------------

-- | Handling of certain events such as kepresses, network signals
appEvent :: BrickEvent Name NetworkEvent -> EventM Name St ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = do
    m <- use mode
    case m of
        Hosting -> do
            mg <- use game
            case mg of
              Just g  -> liftIO (exit g) >> halt
              Nothing -> halt
            halt
        Inputting -> halt
        _         -> mode .= Inputting

-- | Handle Enter
appEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    m <- use mode
    case m of
      Inputting -> do -- if in Inputting state, start joining
        ip   <- unwords <$> use (editIp.to E.getEditContents)
        g    <- liftIO $ startJoining ip
        game .= Just g
        mode .= Connected
      HostingSetup -> do -- if in hosting state, start hosting
        idx <- use ifaceSelected
        ifs <- use ifaces
        g    <- liftIO $ startHosting $ snd $ ifs !! idx
        game .= Just g
        mode .= Hosting
        chan <- use connChan
        liftIO $ void $ forkIO $ Game.waitForClient chan g
      Connected    -> do
        mg   <- use game
        chan <- use connChan
        case mg of
          Just g -> do
            mode    .= Waiting
            liftIO $ Game.sendReady g
            liftIO $ void $ forkIO $ Game.waitForReady chan g
            playerReady .= True
          Nothing  -> return ()
      _            -> return ()

-- | Handle `h` (in inputting)
appEvent (VtyEvent (V.EvKey (V.KFun 1) [])) = do
    m <- use mode
    if m == Inputting then do
        mode .= HostingSetup
        focusRing %= F.focusSetCurrent EditPswd
    else return ()

-- | Handle `tab` (change focus in input)
appEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
    m <- use mode
    if m == Inputting then focusRing %= F.focusNext
    else return ()

-- | Handle `backtab` (change focus in input)
appEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
    m <- use mode
    if m == Inputting then focusRing %= F.focusPrev
    else return ()

-- | Handle ConnectionMade event (when accept() returns)
appEvent (AppEvent ConnectionMade) = do
    mode .= Connected

-- | Handle Ready network event (when opponent declares ready)
appEvent (AppEvent Ready) = do
    m    <- use mode
    mg   <- use game
    case mg of
        Just g  -> liftIO $ sendReady g
        Nothing -> return ()
    if m == Waiting then mode .= Playing
    else enemyReady .= True
    return ()

appEvent (VtyEvent (V.EvKey V.KUp [])) = do
    m <- use mode
    if m == HostingSetup
      then ifaceSelected %= (\i -> max 0 (i-1))
      else cursorPos %= move (-1, 0)

appEvent (VtyEvent (V.EvKey V.KDown [])) = do
    m <- use mode
    if m == HostingSetup
      then do
        ifs <- use ifaces
        ifaceSelected %= (\i -> min (length ifs - 1) (i+1))
      else cursorPos %= move (1, 0)

appEvent (VtyEvent (V.EvKey V.KLeft [])) = cursorPos %= move (0, -1)
appEvent (VtyEvent (V.EvKey V.KRight [])) = cursorPos %= move (0, 1)


appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  cm       <- use cursorMode
  case cm of
    Selecting -> do
      (x, y)   <- use cursorPos
      firstSelect .= (x, y)
      cursorMode  .= Placing
    Placing   -> do
      (x, y)   <- use cursorPos
      (xs, ys) <- use firstSelect
      ships    <- use remainingShips
      board    <- use playerBoard

      case placeShip (x, y) (xs, ys) ships board of
        Just (newBoard, newShips, newShip) -> do
          playerBoard    .= newBoard
          remainingShips .= newShips
          infoMsg .= getPlacedInfo (xs, ys) (x, y)
        Nothing -> infoMsg .= getCannotPlaceInfo (xs, ys) (x, y)
      cursorMode     .= Selecting

-- | Default
appEvent ev = do
    m <- use mode
    case m of
      Inputting -> do -- handle editor events for inputs
        r <- use focusRing
        case F.focusGetCurrent r of
          Just EditIp   -> zoom editIp $ E.handleEditorEvent ev
          Just EditPswd -> zoom editPswd $ E.handleEditorEvent ev
          Nothing -> return ()
      HostingSetup -> do -- handle editor events for hosting setup
        r <- use focusRing
        case F.focusGetCurrent r of
          Just EditPswd -> zoom editPswd $ E.handleEditorEvent ev
          _ -> return ()
      _       -> return ()

-- Cursor logic
appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor st
  | st^.mode == Inputting      = F.focusRingCursor (^. focusRing) st
  | st^.mode == HostingSetup   = F.focusRingCursor (^. focusRing) st
  | otherwise                  = const Nothing

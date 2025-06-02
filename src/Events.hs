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
  , NetworkEvent(..)
  )

import State

import qualified Graphics.Vty as V
import Control.Concurrent (forkIO)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.Types as T

import Lens.Micro
import Lens.Micro.Mtl

import Control.Monad.IO.Class
import Control.Monad (void)

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
        ip <- unwords <$> use (editIp.to E.getEditContents)
        g <- liftIO $ startJoining ip
        game .= Just g
        mode .= Joining
      HostingSetup -> do -- if in hosting state, start hosting
        g <- liftIO startHosting
        game .= Just g
        mode .= Hosting
        chan <- use connChan
        liftIO $ void $ forkIO $ Game.waitForClient chan g
      _            -> return ()

-- | Handle `h` (in inputting)
appEvent (VtyEvent (V.EvKey (V.KFun 1) [])) = do
    m <- use mode
    if m == Inputting then do
        mode .= HostingSetup
        focusRing %= F.focusSetCurrent EditPswd
    else return ()

-- | Handle `tab` (change focus in input)
appEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) =
    focusRing %= F.focusNext

-- | Handle `backtab` (change focus in input)
appEvent (VtyEvent (V.EvKey V.KBackTab [])) =
    focusRing %= F.focusPrev

-- | Handle ConnectionMade event (when accept() returns)
appEvent (AppEvent ConnectionMade) = do
    mode .= Joining
    return ()

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

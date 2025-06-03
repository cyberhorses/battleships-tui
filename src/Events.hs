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

move :: (Int, Int) -> Coord -> Coord
move (dr, dc) (r, c) = (max 0 (min 7 (r+dr)), max 0 (min 7 (c+dc)))

placeShip :: Coord -> Board -> Board
placeShip (r, c) board =
  take r board ++
  [take c (board !! r) ++ [Ship] ++ drop (c+1) (board !! r)] ++
  drop (r+1) board

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
        g    <- liftIO startHosting
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
    return ()

appEvent (VtyEvent (V.EvKey V.KUp [])) = cursorPos %= move (-1, 0)
appEvent (VtyEvent (V.EvKey V.KDown [])) = cursorPos %= move (1, 0)
appEvent (VtyEvent (V.EvKey V.KLeft [])) = cursorPos %= move (0, -1)
appEvent (VtyEvent (V.EvKey V.KRight [])) = cursorPos %= move (0, 1)

appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  (r, c) <- use cursorPos
  playerBoard %= placeShip (r, c)

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

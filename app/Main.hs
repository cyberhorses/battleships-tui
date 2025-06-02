{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V

import Game (Game, startHosting, startJoining, exit, waitForClient, NetworkEvent(..))
import Brick
  ( App(..)
  , BrickEvent(..)
  , EventM
  , Widget
  , halt
  )

import Graphics.Vty.Platform.Unix (mkVty)
import Brick.BChan (BChan, newBChan)
import Control.Concurrent (forkIO)
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Border
import Brick.Widgets.Core

import Control.Monad.IO.Class
import Control.Monad (void)

import Brick.Util (on)

-- Widget Names
data Name = EditIp | EditPswd deriving (Ord, Show, Eq)

-- Modes of the app
data Mode = Inputting | Joining | HostingSetup | Hosting deriving (Eq)

-- App state
data St =
    St { _focusRing :: F.FocusRing Name
       , _editIp    :: E.Editor String Name
       , _editPswd  :: E.Editor String Name
       , _mode      :: Mode
       , _game      :: Maybe Game
       , _connChan  :: BChan NetworkEvent
       }

makeLenses ''St

-- Drawing logic
drawUI :: St -> [Widget Name]
drawUI st = case st^.mode of
    Inputting      -> [drawForm st]
    Joining        -> [drawResult st]
    HostingSetup   -> [drawHostingSetup st]
    Hosting        -> [drawHosting st]

drawForm :: St -> Widget Name
drawForm st =
    C.center . borderWithLabel (str "Join a game") $
        (str "IP Address: " <+> (hLimit 30 $ vLimit 1 eIp)) <=>
        str " " <=>
        (str "Password:   " <+> (hLimit 30 $ vLimit 1 ePswd)) <=>
        str " " <=>
        str "Tab/Shift+Tab to switch fields.\nEnter to submit.\nEsc to quit.\nH to host"
  where
    eIp = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editIp)
    ePswd = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editPswd)

drawResult :: St -> Widget Name
drawResult st =
    C.center . borderWithLabel (str "Reviewing Data") $
        str "You entered:" <=>
        str "" <=>
        str ("IP Address: " ++ ip) <=>
        str ("Password:   " ++ pwd) <=>
        str "" <=>
        str "Press Esc to exit."
  where
    ip = unwords $ E.getEditContents $ st^.editIp
    pwd = unwords $ E.getEditContents $ st^.editPswd

drawHostingSetup :: St -> Widget Name
drawHostingSetup st =
    C.center . borderWithLabel (str "Hosting") $
        (str "Password:   " <+> (hLimit 30 $ vLimit 1 ePswd)) <=>
        str " " <=>
        str "Press ENTER to submit." <=>
        str "" <=>
        str "Press Esc to exit."
  where
    ePswd = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editPswd)

drawHosting :: St -> Widget Name
drawHosting st =
    C.center . borderWithLabel (str "Hosting") $
        (str "Waiting for other players...")

-- Event handler
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

appEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    m <- use mode
    case m of
      Inputting -> do
        ip <- unwords <$> use (editIp.to E.getEditContents)
        g <- liftIO $ startJoining ip
        game .= Just g
        mode .= Joining
      HostingSetup -> do
        g <- liftIO startHosting
        game .= Just g
        mode .= Hosting
        chan <- use connChan
        liftIO $ void $ forkIO $ Game.waitForClient chan g
      _            -> return ()


appEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = do
    m <- use mode
    if m == Inputting then do
        mode .= HostingSetup
        focusRing %= F.focusSetCurrent EditPswd
    else return ()

appEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) =
    focusRing %= F.focusNext

appEvent (VtyEvent (V.EvKey V.KBackTab [])) =
    focusRing %= F.focusPrev

appEvent (AppEvent ConnectionMade) = do
    mode .= Joining
    return ()

appEvent ev = do
    m <- use mode
    case m of
      Inputting -> do
        r <- use focusRing
        case F.focusGetCurrent r of
          Just EditIp   -> zoom editIp $ E.handleEditorEvent ev
          Just EditPswd -> zoom editPswd $ E.handleEditorEvent ev
          Nothing -> return ()
      HostingSetup -> do
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


-- Initial state
initialState :: BChan NetworkEvent -> St
initialState chan =
    St (F.focusRing [EditIp, EditPswd])
       (E.editor EditIp (Just 1) "")
       (E.editor EditPswd (Just 1) "")
       Inputting
       Nothing
       chan

-- AttrMap
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,        V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

-- App definition
theApp :: App St NetworkEvent Name
theApp = App
    { appDraw         = drawUI
    , appChooseCursor = appCursor
    , appHandleEvent  = appEvent
    , appStartEvent   = return ()
    , appAttrMap      = const theMap
    }

-- Main
main :: IO ()
main = do
    chan <- newBChan 10
    let initSt = initialState chan

    vty <- mkVty V.defaultConfig
    finalState <- M.customMain vty (mkVty V.defaultConfig) (Just chan) theApp initSt

    -- 5) After the UI quits, print the IP/Password
    putStrLn $ "IP Address: " ++ unwords (E.getEditContents $ finalState^.editIp)
    putStrLn $ "Password:   " ++ unwords (E.getEditContents $ finalState^.editPswd)


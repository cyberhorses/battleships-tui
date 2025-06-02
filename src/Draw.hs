{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

-- Functions for drawing TUI scenes

module Draw
  ( drawUI
  , drawForm
  , drawResult
  , drawHostingSetup
  , drawHosting
  ) where

import State
import Brick(Widget)
import Lens.Micro

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Brick.Widgets.Border
import Brick.Widgets.Core

-- Drawing logic
-- Draw function for each mode (window)
drawUI :: St -> [Widget Name]
drawUI st = case st^.mode of
    Inputting      -> [drawForm st]
    Joining        -> [drawResult st]
    HostingSetup   -> [drawHostingSetup st]
    Hosting        -> [drawHosting st]

-- Draw the initial input form
drawForm :: St -> Widget Name
drawForm st =
    C.center . borderWithLabel (str "Join a game") $
        (str "IP Address: " <+> (hLimit 30 $ vLimit 1 eIp)) <=>
        str " " <=>
        (str "Password:   " <+> (hLimit 30 $ vLimit 1 ePswd)) <=>
        str " " <=>
        str "TAB to switch fields. Enter to submit. Esc to quit. 'F1' to host"
  where
    eIp = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editIp)
    ePswd = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editPswd)

-- Draw the results (to be changed, this gets drawn after joining/hosting)
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

-- Draw the hosting setup window
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

-- Draw the waiting for connection screen
drawHosting :: St -> Widget Name
drawHosting _ =
    C.center . borderWithLabel (str "Hosting") $
        (str "Waiting for other players...")

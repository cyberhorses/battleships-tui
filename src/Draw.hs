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
  , drawConnected
  , drawWaiting
  ) where

import Ships
import State
import Brick(Widget)
import Lens.Micro

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Center (center)
import Data.Char  
import Brick.AttrMap (attrName)

import qualified Data.Map.Strict as M
import qualified Data.List as L


-- Drawing logic
-- Draw function for each mode (window)
drawUI :: St -> [Widget Name]
drawUI st = [mainDisp <=> drawInfoMsg st]
  where
    mainDisp = case st^.mode of
      Inputting      -> drawForm st
      Joining        -> drawResult st
      HostingSetup   -> drawHostingSetup st
      Hosting        -> drawHosting st
      Connected      -> drawConnected st
      Waiting        -> drawWaiting st
      Playing        -> drawPlaying st

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
      vBox
        [ str "Host on:"
        , vBox $ zipWith drawIface [0..] (st^.ifaces)
        , str " "
        , str "Password:   " <+> (hLimit 30 $ vLimit 1 ePswd)
        , str " "
        , str "Press ENTER to submit."
        , str ""
        , str "Press Esc to exit."
        ]
  where
    ePswd = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editPswd)
    sel = st^.ifaceSelected
    drawIface i (n, ip) =
      (if i == sel then withAttr (attrName "editFocusedAttr") else id) $
        str $ " • " ++ n ++ ": " ++ ip

-- Draw the waiting for connection screen
drawHosting :: St -> Widget Name
drawHosting st =
    let idx = st^.ifaceSelected
        ifs = st^.ifaces
        ip = snd (ifs !! idx)
    in C.center . borderWithLabel (str "Hosting") $
         vBox
           [ str $ "Hosting on: " ++ ip
           , str " "
           , str "Waiting for other player..."
           ]
    


drawConnected :: St -> Widget Name
drawConnected st =
  C.center $
    vBox
        [
          C.center $ hBox
            [ (borderWithLabel (str "Your Board") $ drawBoard (st^.playerBoard) (Just (st^.cursorPos)))
            , str " "
            , drawShipsLeft (st^.remainingShips)
            ]
          , str "Use arow keys to move the cursor, Press SPACE to start placing"
      ]

-- placeholder
enemyBoard :: Board
enemyBoard = replicate 10 (replicate 10 Empty)  -- ~ = unknown

-- Draw one board with label
drawBoard :: Board -> Maybe Coord -> Widget Name
drawBoard board mCursor =
  vBox $
    [ hBox (str "   " : [str (" " ++ [chr (ord 'A' + c)]) | c <- [0..9]]) ] ++
    [ hBox (str (rowLabel r) : [drawCell r c | c <- [0..9]])
    | r <- [0..9]
    ]
  where
    rowLabel r
      | r < 9     = " " ++ show (r+1) ++ " "
      | otherwise = show (r+1) ++ " "
    drawCell r c =
      let cell = board !! r !! c
          symbol = case cell of
            Empty -> "~"
            Ship  -> "✕"
            Hit   -> "✖"
            Miss  -> "○"
          baseAttr = case cell of
            Empty -> attrName "water"
            Ship  -> attrName "ship"
            Hit   -> attrName "hit"
            Miss  -> attrName "miss"
          finalAttr = if Just (r, c) == mCursor
                      then attrName "cursor"
                      else baseAttr
      in withAttr finalAttr (str (" " ++ symbol))
    --cellChar Empty = str "~"
    --cellChar Ship  = str "✕"
    --cellChar Hit   = str "✖"
    --cellChar Miss  = str "○"

drawShipsLeft :: RemainingShips -> Widget Name
drawShipsLeft ships = do
  if allShipsPlaced ships then
    borderWithLabel (str "Ships Left") $ str "None, press ENTER to continue"
  else
    borderWithLabel (str "Ships Left") $
      vBox $ map renderShip (L.sortOn fst $ M.toList ships)
  where
    renderShip (len, count) =
      str $ show count ++ "x length-" ++ show len
    allShipsPlaced = all (== 0) . M.elems

drawPlaying :: St -> Widget Name
drawPlaying st =
  C.center $
    vBox
      [
        str "MAIN GAME SCREEN PLACEHOLDER"
      ]

drawWaiting :: St -> Widget Name
drawWaiting st =
  C.center $
    vBox
      [ border $ drawBoard (st^.playerBoard) (Just (st^.cursorPos))
      , str "Waiting for other player..."
      ]


drawInfoMsg :: St -> Widget Name
drawInfoMsg st =
  if null (st^.infoMsg)
    then str ""
    else str $ getInfoMsg st

getInfoMsg :: St -> String
getInfoMsg st =
  let msg = st^.infoMsg
  in if null msg
       then ""
       else "INFO: " ++ msg
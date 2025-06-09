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
import MsgHandle
import Config
import Brick(Widget)
import Lens.Micro

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.Center (hCenter)
import Data.Char  
import Brick.AttrMap (attrName)

import qualified Data.Map.Strict as M
import qualified Data.List as L


-- Drawing logic
-- Draw function for each mode (window)
drawUI :: St -> [Widget Name]
drawUI st = [mainDisp]
  where
    mainDisp = case st^.mode of
      Inputting      -> drawForm st
      Joining        -> drawResult st
      HostingSetup   -> drawHostingSetup st
      Hosting        -> drawHosting st
      Connected      -> drawConnected st
      Waiting        -> drawWaiting st
      Playing        -> drawPlaying st
      Finished       -> drawFinished st
      

-- Draw the initial input form
drawForm :: St -> Widget Name
drawForm st =
    C.center . borderWithLabel (str "Join a game") $
        str " " <=>
        (str "IP Address: " <+> (hLimit 30 $ vLimit 1 eIp)) <=>
        str " " <=>
        str "TAB to switch fields. Enter to submit. Esc to quit. 'F1' to host"
  where
    eIp = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.editIp)

-- Draw the results (to be changed, this gets drawn after joining/hosting)
drawResult :: St -> Widget Name
drawResult st =
    C.center . borderWithLabel (str "Reviewing Data") $
        str "You entered:" <=>
        str "" <=>
        str ("IP Address: " ++ ip) <=>
        str "" <=>
        str "Press Esc to exit."
  where
    ip = unwords $ E.getEditContents $ st^.editIp

-- Draw the hosting setup window
drawHostingSetup :: St -> Widget Name
drawHostingSetup st =
    C.center . borderWithLabel (str "Hosting") $
      vBox
        [ str "Host on:"
        , vBox $ zipWith drawIface [0..] (st^.ifaces)
        , str " "
        , str "Press ENTER to submit."
        , str ""
        , str "Press Esc to exit."
        ]
  where
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
      [ hBox
        [ drawYourShips (st^.playerShips) (length (st^.playerBoard) + 2)
        , (borderWithLabel (str "Your Board") $ drawBoard (st^.playerBoard) (Just (st^.cursorPos)))
        , str " "
        , drawShipsLeft (st^.remainingShips)
        ]
      , str "Use arow keys to move the cursor, Press SPACE to start placing"
      , drawInfoMsg (st^.infoMsg)
      ]


-- Draw one board with label
drawBoard :: Board -> Maybe Coord -> Widget Name
drawBoard board mCursor =
  vBox $
    [ hBox (str "   " : [str ([chr (ord 'A' + c)] ++ " ") | c <- [0..(mapSize - 1) ]]) ] ++
    [ hBox (str (rowLabel r) : [drawCell r c | c <- [0..(mapSize - 1)]])
    | r <- [0..(mapSize - 1)]
    ]
  where
    rowLabel r
      | r < mapSize - 1     = " " ++ show (r+1) ++ " "
      | otherwise           = show (r+1) ++ " "
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
      in withAttr finalAttr (str (symbol ++ " "))
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
        [ withAttr (attrName "bold") $ hCenter $ str turnMsg
        , C.center $ hBox
            [ drawYourShips (st^.playerShips) (length (st^.playerBoard) + 2)
            , (borderWithLabel (str "Your Board") $ drawBoard (st^.playerBoard) (Nothing))
            , str " "
            , (borderWithLabel (str "Opponet's Board") $ drawBoard (st^.opponentBoard) (Just (st^.cursorPos)))
            , str " "
            , drawShipsLeft (st^.remainingShips)
            ]
          , str "Use arow keys to move the cursor, Press SPACE to shoot"
          , drawInfoMsg (st^.infoMsg)
      ]
  where
    turnMsg = if st^.playerTurn then "Your Turn" else "Your Opponent's Turn"

drawFinished :: St -> Widget Name
drawFinished st =
  C.center $
    border $
      hLimit 40 $
        vLimit 9 $
          vBox
            [ C.hCenter $ withAttr (attrName $ if st^.gameWon then "win" else "lose") $
                str ("   " ++ label ++ "   ")
            , padTop (Pad 1) $ C.hCenter $ withAttr (attrName "bold") $ str text
            , padTop (Pad 2) $ C.hCenter $ str "Press Escape to return to menu"
            ]
  where
    label = if st^.gameWon then "WINNER" else "LOSER"
    text  = if st^.gameWon then "Congratulations!!!" else "Better luck next time!"

drawWaiting :: St -> Widget Name
drawWaiting st =
  C.center $
    vBox
      [ border $ drawBoard (st^.playerBoard) (Just (st^.cursorPos))
      , str "Waiting for other player..."
      ]


drawInfoMsg :: String -> Widget Name
drawInfoMsg msg =
  if null msg
    then str ""
    else str $ "INFO: " ++ msg


drawYourShips :: ShipMap -> Int -> Widget Name
drawYourShips shipMap boardHeight =
  if M.null shipMap
    then str "" 
    else vLimit boardHeight $
      borderWithLabel (str "Your ships:") $
        vBox $ map renderShip (M.toList shipMap)
  where
    renderShip (ship, lifes) =
      let c1 = showCoord (start ship)
          c2 = showCoord (end ship)
          l  = len ship
      in str $ "(" ++ c1 ++ ", " ++ c2 ++ ")  " ++ show lifes ++ "/" ++ show l


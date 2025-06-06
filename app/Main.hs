{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lens.Micro
import qualified Graphics.Vty as V

import Game (NetworkEvent(..), getIPv4Interfaces)  -- Main Game

import Brick
  ( App(..)
  )

import Events  -- Event handling (button press, network etc.)
import State   -- Current app state
import Draw    -- Drawing TUI scenes

import Graphics.Vty.Platform.Unix (mkVty)
import Brick.BChan (BChan, newBChan)
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M

import Brick.Util (on, fg)


-- Initial state
initialState :: BChan NetworkEvent -> Interfaces -> St
initialState chan interfaces =
    St (F.focusRing [EditIp, EditPswd])
       (E.editor EditIp (Just 1) "")
       (E.editor EditPswd (Just 1) "")
       interfaces
       0
       Inputting
       Nothing
       chan
       playerEmptyBoard
       (0, 0)
       (0, 0)
       Selecting
       defaultShips
       False
       False

-- AttrMap
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,        V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , (A.attrName "editFocusedAttr", V.black `on` V.yellow)  -- <-- dodaj to!
    , (A.attrName "cursor", V.withStyle V.defAttr V.standout `V.withStyle` V.reverseVideo)
    , (A.attrName "water",  fg V.blue)
    , (A.attrName "ship",   fg V.cyan)
    , (A.attrName "hit",    fg V.red)
    , (A.attrName "miss",   fg V.white)
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
    interfaces <- getIPv4Interfaces
    let initSt = initialState chan interfaces

    vty <- mkVty V.defaultConfig
    finalState <- M.customMain vty (mkVty V.defaultConfig) (Just chan) theApp initSt

    -- 5) After the UI quits, print the IP/Password
    putStrLn $ "IP Address: " ++ unwords (E.getEditContents $ finalState^.editIp)
    putStrLn $ "Password:   " ++ unwords (E.getEditContents $ finalState^.editPswd)

    putStrLn "Interfases:"
    mapM_ (\(n, ip) -> putStrLn $ n ++ ": " ++ ip) (finalState^.ifaces)



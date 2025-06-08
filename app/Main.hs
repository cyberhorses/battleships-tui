{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lens.Micro
import qualified Graphics.Vty as V

import GameSession (NetworkEvent(..), getIPv4Interfaces)  -- Main Game

import Brick
  ( App(..)
  )
import System.IO
import Events  -- Event handling (button press, network etc.)
import State   -- Current app state
import Draw    -- Drawing TUI scenes

import Graphics.Vty.Platform.Unix (mkVty)
import Brick.BChan (newBChan)
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Main as M

import Brick.Util (on, fg)



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
    , (A.attrName "bold", V.withStyle V.defAttr V.bold)
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
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    
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



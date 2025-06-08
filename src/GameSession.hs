{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: Rename this to Network.hs, and change all associated functions

module GameSession
  ( GameSession(..)
  , startHosting
  , startJoining
  , mainListener
  , waitForClient
  , sendReady
  , exit
  , respondToShot
  , sendShot
  , sendShipDown
  , sendNoShipsLeft
  , getIPv4Interfaces
  , debugLog
  , NetworkEvent(..)
  ) where

import Ships
import Config
import MsgHandle
import Network.Socket
import Network.Info
import Data.IORef
import Brick.BChan (BChan, writeBChan)
import Data.List (dropWhileEnd)
import Data.List.Split (splitOneOf)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket.ByteString as NSB

data GameSession = GameSession
    { gameSocket        :: Socket
    , opponentSocket  :: IORef (Maybe Socket)
    }

-- Network events
data NetworkEvent = ConnectionMade
                  | Ready
                  | RecievedShot Coord
                  | ShotResponse Cell Coord
                  | ShipDownMessage Coord Coord ShipLen
                  | NoShipsLeftMessage
                  | UnknownMessage



startHosting :: String -> IO GameSession
startHosting ip = do
    addrinfos <- getAddrInfo
                   (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                   (Just ip)
                   (Just gamePort)
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock 1

    opponentRef <- newIORef Nothing
    pure $ GameSession sock opponentRef

startJoining :: String -> IO GameSession
startJoining ip = do
    addrinfos <- getAddrInfo Nothing (Just ip) (Just gamePort)
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)

    putStrLn $ "Connected to host at " ++ ip
    
    opponentRef <- newIORef (Just sock)
    pure $ GameSession sock opponentRef

waitForClient :: BChan NetworkEvent -> GameSession -> IO ()
waitForClient chan GameSession{..} = do
    (connSock, clientAddr) <- accept gameSocket
    debugLog $ "Connection from " ++ show clientAddr
    writeIORef opponentSocket (Just connSock)
    writeBChan chan ConnectionMade


exit :: GameSession -> IO ()
exit GameSession{..} = do
    close gameSocket
    mOpponent <- readIORef opponentSocket
    case mOpponent of
        Just sock -> close sock
        Nothing -> return ()

mainListener :: BChan NetworkEvent -> GameSession -> IO ()
mainListener chan GameSession{..} = do
    mSock <- readIORef opponentSocket
    case mSock of
      Just sock -> loop sock ""
      Nothing   -> debugLog "No opponent socket found."
  where
    loop sock leftover = do
      msg <- NSB.recv sock 1024
      let allData = leftover ++ BS.unpack msg
          msgs = splitOneOf "\r\n" allData
      mapM_ handleMsg (init msgs)
      let rest = if null msgs then "" else last msgs
      loop sock rest
    handleMsg rawStr =
      let str = dropWhileEnd isSpace rawStr
      in case () of
        _ | str == readyStr ->
              writeBChan chan Ready
          | str == noShipsLeftStr ->
              writeBChan chan NoShipsLeftMessage
          | Just coord <- parseShot str -> 
              writeBChan chan (RecievedShot coord)
          | Just (cell, coord) <- parseShotResponse str -> 
              writeBChan chan (ShotResponse cell coord)
          | Just (crd1, crd2, len) <- parseShipDown str ->
                writeBChan chan (ShipDownMessage crd1 crd2 len)
          | otherwise -> 
              debugLog $ "handleMsg: Unknown message: " ++ show str


sendReady :: GameSession -> IO ()
sendReady GameSession{..} = do
    mSock <- readIORef opponentSocket
    case mSock of
        Just sock -> NSB.sendAll sock (BS.pack readyMsg)
        Nothing   -> debugLog "Cannot send READY: No opponent socket."

respondToShot :: GameSession -> Cell -> Coord -> IO ()
respondToShot GameSession{..} result coord = do
    mSock <- readIORef opponentSocket
    let msg = shotResponseMsg result coord
    case mSock of
      Just sock -> NSB.sendAll sock (BS.pack msg)
      Nothing   -> debugLog "respondToShot: No opponent socket found."

sendShot :: GameSession -> Coord -> IO ()
sendShot GameSession{..} c = do
    mSock <- readIORef opponentSocket
    case mSock of
      Just sock -> NSB.sendAll sock (BS.pack $ shotMsg c)
      Nothing -> debugLog "No opponent socket found."

sendShipDown :: GameSession -> UserShip -> IO ()
sendShipDown GameSession{..} ship = do
    mSock <- readIORef opponentSocket
    let msg = shipDownMsg ship
    case mSock of
      Just sock -> NSB.sendAll sock (BS.pack msg)
      Nothing   -> debugLog "sendShipDown: No opponent socket found."

sendNoShipsLeft :: GameSession -> IO ()
sendNoShipsLeft GameSession{..} = do
    mSock <- readIORef opponentSocket
    case mSock of
      Just sock -> NSB.sendAll sock (BS.pack noShipsLeftMsg)
      Nothing   -> debugLog "sendNoShipsLeft: No opponent socket found."

getIPv4Interfaces :: IO [(String, String)]
getIPv4Interfaces = do
    ifaces <- getNetworkInterfaces
    pure [ (name i, show (ipv4 i)) | i <- ifaces, show (ipv4 i) /= "0.0.0.0" ]

debugLog :: String -> IO ()
debugLog msg = appendFile "debug.log" (msg ++ "\n")
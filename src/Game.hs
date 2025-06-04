{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: Rename this to Network.hs, and change all associated functions

module Game
  ( Game(..)
  , startHosting
  , startJoining
  , waitForClient
  , waitForReady
  , sendReady
  , exit
  , NetworkEvent(..)
  ) where

import Network.Socket
import Data.IORef
import Brick.BChan (BChan, writeBChan)
import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket.ByteString as NSB

data Game = Game
    { gameSocket        :: Socket
    , opponentSocket  :: IORef (Maybe Socket)
    }

-- Network events
data NetworkEvent = ConnectionMade
                  | Ready


startHosting :: IO Game
startHosting = do
    addrinfos <- getAddrInfo
                   (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                   Nothing
                   (Just "3000")
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)
    listen sock 1

    opponentRef <- newIORef Nothing
    pure $ Game sock opponentRef

startJoining :: String -> IO Game
startJoining ip = do
    addrinfos <- getAddrInfo Nothing (Just ip) (Just "3000")
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)

    putStrLn $ "Connected to host at " ++ ip
    
    opponentRef <- newIORef (Just sock)
    pure $ Game sock opponentRef

waitForClient :: BChan NetworkEvent -> Game -> IO ()
waitForClient chan Game{..} = do
    (connSock, clientAddr) <- accept gameSocket
    putStrLn $ "Connection from " ++ show clientAddr
    writeIORef opponentSocket (Just connSock)
    writeBChan chan ConnectionMade


exit :: Game -> IO ()
exit Game{..} = do
    close gameSocket
    mOpponent <- readIORef opponentSocket
    case mOpponent of
        Just sock -> close sock
        Nothing -> return ()

sendReady :: Game -> IO ()
sendReady Game{..} = do
    mSock <- readIORef opponentSocket
    case mSock of
        Just sock -> NSB.sendAll sock (BS.pack "READY")
        Nothing   -> putStrLn "Cannot send READY: No opponent socket."


waitForReady :: BChan NetworkEvent -> Game -> IO ()
waitForReady chan Game{..} = do
    mSock <- readIORef opponentSocket
    case mSock of
        Just sock -> do
            msg <- NSB.recv sock 1024
            putStrLn $ "MESSAGE: " ++ BS.unpack msg
            if BS.unpack msg == "READY"
              then do
                sendReady Game{..}
                writeBChan chan Ready
              else waitForReady chan Game{..}  -- keep listening
        Nothing -> putStrLn "No opponent socket found."

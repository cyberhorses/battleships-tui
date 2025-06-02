{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game
  ( Game(..)
  , startHosting
  , startJoining
  , waitForClient
  , exit
  , NetworkEvent(..)
  ) where

import Network.Socket
import Data.IORef
import Brick.BChan (BChan, writeBChan)

data Game = Game
    { gameSocket        :: Socket
      , opponentSocket  :: IORef (Maybe Socket)
    }

-- Network events
data NetworkEvent = ConnectionMade


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
    
    opponentRef <- newIORef Nothing  -- get opponent here
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

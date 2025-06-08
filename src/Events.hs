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

import GameSession

import State
import MsgHandle

import qualified Graphics.Vty as V
import Control.Concurrent (forkIO)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.Types as T

import Ships
import Lens.Micro
import Lens.Micro.Mtl

import Control.Monad.IO.Class
import Control.Monad (void, when)


---- Event handler -------------------------------------------

-- | Handling of certain events such as kepresses, network signals
appEvent :: BrickEvent Name NetworkEvent -> EventM Name St ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = do
    m <- use mode
    case m of
        Hosting -> backToMenu
        Connected -> backToMenu
        Waiting -> backToMenu
        Playing -> backToMenu
        Finished -> backToMenu
        Inputting -> halt
        _         -> mode .= Inputting

-- | Handle Enter
appEvent (VtyEvent (V.EvKey V.KEnter [])) = do
    m <- use mode
    case m of
      Inputting     -> handleEnterInputting
      HostingSetup  -> handleEnterHostingSetup
      Connected     -> handleEnterConnected
      _             -> return()

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

-- | UP ARROW
appEvent (VtyEvent (V.EvKey V.KUp [])) = do
    m <- use mode
    case m of
      HostingSetup -> ifaceSelected %= (\i -> max 0 (i-1))
      Connected    -> cursorPos %= move (-1, 0)
      Playing      -> cursorPos %= move (-1, 0)
      _            -> return ()

-- | DOWN ARROW
appEvent (VtyEvent (V.EvKey V.KDown [])) = do
    m <- use mode
    case m of
      HostingSetup -> do
        ifs <- use ifaces
        ifaceSelected %= (\i -> min (length ifs - 1) (i+1))
      Connected    -> cursorPos %= move (1, 0)
      Playing      -> cursorPos %= move (1, 0)
      _            -> return ()

-- | LEFT ARROW
appEvent (VtyEvent (V.EvKey V.KLeft [])) =  do 
  m <- use mode
  case m of 
    Connected -> cursorPos %= move (0, -1)
    Playing   -> cursorPos %= move (0, -1)
    _         -> return()

appEvent (VtyEvent (V.EvKey V.KRight [])) = do 
  m <- use mode
  case m of 
    Connected -> cursorPos %= move (0, 1)
    Playing   -> cursorPos %= move (0, 1)
    _         -> return()


appEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
  cm       <- use cursorMode
  case cm of
    Selecting -> handleSpaceSelecting
    Placing   -> handleSpacePlacing
    Shooting  -> handleSpaceShooting

-- ------------------------------------------------------------------------
-- NETWORK EVENTS
-- ------------------------------------------------------------------------

-- | Handle ConnectionMade event (when accept() returns)
appEvent (AppEvent ConnectionMade) = do
    mode .= Connected
    chan <- use connChan
    mg   <- use game
    case mg of
      Just g  -> liftIO $ void $ forkIO $ mainListener chan g
      Nothing -> return ()

-- | Handle Ready network event (when opponent declares ready)
appEvent (AppEvent Ready) = do
    m <- use mode
    enemyReady .= True
    case m of
      Waiting -> tryStartGame
      _       -> return ()


appEvent (AppEvent (RecievedShot coord)) = do
    ships <- use playerShips
    board <- use playerBoard
    let (result, newShipMap, newBoard, mShip) = handleShot coord ships board
    playerBoard .= newBoard
    playerShips .= newShipMap
    infoMsg     .= getOppShotInfo result coord
    
    mg <- use game
    case mg of
      Just g  -> do
        liftIO $ respondToShot g result coord
        case mShip of
          Just ship -> do 
            liftIO $ sendShipDown g ship
            infoMsg .= getShipSunkInfo "Your" (start ship) (end ship) (len ship)
          Nothing -> return()
      Nothing -> return ()
    playerTurn  .= True
    when (allShipsSunk newShipMap) handleLost

appEvent (AppEvent (ShotResponse cell coord)) = do
    opponentBoard %= \board -> setCell board coord cell
    infoMsg .= getShotResultInfo cell coord

appEvent (AppEvent (ShipDownMessage crd1 crd2 len)) = do
    infoMsg .= getShipSunkInfo "Opponent's " crd1 crd2 len
    oppBoard <- use opponentBoard
    remShips <- use remainingShips

    opponentBoard  .= handleSunkShip oppBoard crd1 crd2
    remainingShips .= decrementShips len remShips

appEvent (AppEvent NoShipsLeftMessage) = do 
    mode    .= Finished
    gameWon .= True

-- | Default
appEvent ev = do
    m <- use mode
    case m of
      Inputting -> do -- handle editor events for inputs
        r <- use focusRing
        case F.focusGetCurrent r of
          Just EditIp   -> zoom editIp $ E.handleEditorEvent ev
          Just EditPswd -> zoom editPswd $ E.handleEditorEvent ev
          _         -> return()
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


processShot :: GameSession -> Coord -> EventM Name St ()
processShot g coord = do
    liftIO $ sendShot g coord

-- ------------------------------------------------------------------------
-- SPECIFIC HANDLERS
-- ------------------------------------------------------------------------
-- | ENTER
handleEnterInputting :: EventM Name St ()
handleEnterInputting = do
    ip   <- unwords <$> use (editIp.to E.getEditContents)
    g    <- liftIO $ startJoining ip
    game .= Just g
    mode .= Connected
    chan <- use connChan
    liftIO $ void $ forkIO $ GameSession.mainListener chan g

handleEnterHostingSetup :: EventM Name St ()
handleEnterHostingSetup = do
    idx <- use ifaceSelected
    ifs <- use ifaces
    g    <- liftIO $ startHosting $ snd $ ifs !! idx
    playerTurn .= False
    game .= Just g
    mode .= Hosting
    chan <- use connChan
    liftIO $ void $ forkIO $ GameSession.waitForClient chan g

handleEnterConnected :: EventM Name St ()
handleEnterConnected = do
    ships <- use remainingShips
    if not (null ships)
      then infoMsg .= "You have to place all ships to continue"
      else do
        remainingShips .= defaultShips
        mg <- use game
        case mg of
          Nothing -> return ()
          Just g -> do
            liftIO $ GameSession.sendReady g
            playerReady .= True
            tryStartGame

-- | SPACE
handleSpaceSelecting :: EventM Name St ()
handleSpaceSelecting = do
    (x, y)   <- use cursorPos
    firstSelect .= (x, y)
    cursorMode  .= Placing

handleSpacePlacing :: EventM Name St ()
handleSpacePlacing = do
    (x, y)   <- use cursorPos
    (xs, ys) <- use firstSelect
    ships    <- use remainingShips
    board    <- use playerBoard

    case placeShip (x, y) (xs, ys) ships board of
      Just (newBoard, newShips, newShip) -> do
        playerBoard    .= newBoard
        remainingShips .= newShips
        playerShips    %= addShipToMap newShip
        infoMsg .= getPlacedInfo (xs, ys) (x, y)
      Nothing -> infoMsg .= getCannotPlaceInfo (xs, ys) (x, y)
    cursorMode .= Selecting

handleSpaceShooting :: EventM Name St ()
handleSpaceShooting = do
    isMyTurn <- use playerTurn
    mg <- use game
    (x, y) <- use cursorPos
    board <- use opponentBoard
    if not (canShootAt board (x, y))
      then infoMsg .= getCannotShootInfo (x, y)
      else if not isMyTurn
        then infoMsg .= getNotYourTurnInfo
        else case mg of
          Just g  -> do 
            processShot g (x, y)
            playerTurn .= False
          Nothing -> return ()

-- ------------------------------------------------------------------------
-- HELPERS
-- ------------------------------------------------------------------------
tryStartGame :: EventM Name St ()
tryStartGame = do
    pr <- use playerReady
    er <- use enemyReady
    if pr && er
      then do
        mode .= Playing
        cursorMode .= Shooting
      else mode .=Waiting

handleLost :: EventM Name St ()
handleLost = do
    mode    .= Finished
    gameWon .= False
    mg <- use game
    case mg of
      Just g  -> liftIO $ sendNoShipsLeft g
      Nothing -> return ()

backToMenu :: EventM Name St ()
backToMenu = do
    chan <- use connChan
    ifs  <- use ifaces
    T.put (initialState chan ifs)
    mg <- use game
    case mg of
      Just g  -> liftIO (exit g)
      Nothing -> return()
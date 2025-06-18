{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module State
  ( St(..)
  , Mode(..)
  , Name(..)
  , focusRing
  , editIp
  , editPswd
  , ifaces
  , ifaceSelected
  , mode
  , game
  , connChan
  , playerBoard
  , playerShips
  , opponentBoard 
  , cursorPos
  , firstSelect
  , cursorMode
  , remainingShips
  , defaultShips
  , playerReady
  , enemyReady
  , infoMsg
  , playerTurn
  , gameWon
  , initialState
  , Board
  , CursorMode(..)
  , Coord
  , Cell(..)
  , RemainingShips
  , Interfaces
  ) where

import Lens.Micro.TH

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import GameSession(GameSession(..), NetworkEvent(..))
import Ships
import Brick.BChan (BChan)

-- Widget Names
data Name = EditIp | EditPswd deriving (Ord, Show, Eq)

-- Modes of the app
data Mode = Inputting
          | Joining
          | HostingSetup
          | Hosting
          | Connected
          | Waiting
          | Playing
          | Finished
          deriving (Eq)

data CursorMode = Selecting
                | Placing
                | Shooting

type Interfaces = [(String, String)]

-- App state
data St =
    St { _focusRing      :: F.FocusRing Name      -- Brick's focus rings (for tracking focus on input fields)
       , _editIp         :: E.Editor String Name  -- IP Edit field
       , _editPswd       :: E.Editor String Name  -- password edit field
       , _ifaces         :: Interfaces            -- interface (name, ip)
       , _ifaceSelected  :: Int                   -- index of interface selected
       , _mode           :: Mode                  -- currently displayed screen
       , _game           :: Maybe GameSession            -- the game
       , _connChan       :: BChan NetworkEvent    -- connection event channel
       , _playerBoard    :: Board                 -- the player's board
       , _playerShips    :: ShipMap               -- player's ships with lifes
       , _opponentBoard  :: Board                 -- the opponent's board
       , _cursorPos      :: Coord                 -- where the cursor is for placing ships
       , _firstSelect    :: Coord                 -- where the cursor is for placing ships
       , _cursorMode     :: CursorMode            -- in what mode our cursor currently is on the grid
       , _remainingShips :: RemainingShips        -- how many more ships we can place/later as enemy ships
       , _playerReady    :: Bool
       , _enemyReady     :: Bool
       , _infoMsg        :: String
       , _playerTurn     :: Bool                  -- joiner has priority
       , _gameWon        :: Bool
       }

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
       emptyBoard
       emptyShipMap
       emptyBoard
       (0, 0)
       (0,0)
       Selecting
       defaultShips
       False
       False
       ""
       True -- joiner firs
       False

makeLenses ''St


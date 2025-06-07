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
  , opponentBoard 
  , cursorPos
  , firstSelect
  , cursorMode
  , remainingShips
  , defaultShips
  , playerReady
  , enemyReady
  , infoMsg
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
import qualified Data.Map as M

import Game(Game(..), NetworkEvent(..))
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
          deriving (Eq)

data CursorMode = Selecting
                | Placing

type Interfaces = [(String, String)]

-- App state
data St =
    St { _focusRing      :: F.FocusRing Name      -- Brick's focus rings (for tracking focus on input fields)
       , _editIp         :: E.Editor String Name  -- IP Edit field
       , _editPswd       :: E.Editor String Name  -- password edit field
       , _ifaces         :: Interfaces            -- interface (name, ip)
       , _ifaceSelected  :: Int                   -- index of interface selected
       , _mode           :: Mode                  -- currently displayed screen
       , _game           :: Maybe Game            -- the game
       , _connChan       :: BChan NetworkEvent    -- connection event channel
       , _playerBoard    :: Board                 -- the player's board
       , _opponentBoard  :: Board                 -- the opponent's board
       , _cursorPos      :: Coord                 -- where the cursor is for placing ships
       , _firstSelect    :: Coord                 -- where the cursor is for placing ships
       , _cursorMode     :: CursorMode            -- in what mode our cursor currently is on the grid
       , _remainingShips :: RemainingShips        -- how many more ships we can place
       , _playerReady    :: Bool
       , _enemyReady     :: Bool
       , _infoMsg        :: String
       }

makeLenses ''St


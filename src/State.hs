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
  , mode
  , game
  , connChan
  , playerBoard
  , cursorPos
  , firstSelect
  , cursorMode
  , remainingShips
  , defaultShips
  , playerReady
  , enemyReady
  , Board
  , CursorMode(..)
  , Coord
  , Cell(..)
  , RemainingShips
  ) where

import Lens.Micro.TH

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Data.Map as M

import Game(Game(..), NetworkEvent(..))
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

type Coord = (Int, Int)  -- (row, col)

data Cell = Empty | Ship | Hit | Miss deriving (Eq)
type Board = [[Cell]]  -- 8x8 grid

type RemainingShips = M.Map Int Int

defaultShips :: RemainingShips
defaultShips = M.fromList
  [ (2, 1)
  , (3, 2)
  , (4, 1)
  , (5, 1)
  ]

-- App state
data St =
    St { _focusRing      :: F.FocusRing Name      -- Brick's focus rings (for tracking focus on input fields)
       , _editIp         :: E.Editor String Name  -- IP Edit field
       , _editPswd       :: E.Editor String Name  -- password edit field
       , _mode           :: Mode                  -- currently displayed screen
       , _game           :: Maybe Game            -- the game
       , _connChan       :: BChan NetworkEvent    -- connection event channel
       , _playerBoard    :: Board                 -- the player's board
       , _cursorPos      :: Coord                 -- where the cursor is for placing ships
       , _firstSelect    :: Coord                 -- where the cursor is for placing ships
       , _cursorMode     :: CursorMode            -- in what mode our cursor currently is on the grid
       , _remainingShips :: RemainingShips        -- how many more ships we can place
       , _playerReady    :: Bool
       , _enemyReady     :: Bool
       }

makeLenses ''St


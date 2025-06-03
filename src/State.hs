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
  , Board
  , BoardMode(..)
  , Coord
  , Cell(..)
  ) where

import Lens.Micro.TH

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

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
          deriving (Eq)

data BoardMode = Selecting
               | Placing

type Coord = (Int, Int)  -- (row, col)

data Cell = Empty | Ship | Hit | Miss deriving (Eq)
type Board = [[Cell]]  -- 8x8 grid

-- App state
data St =
    St { _focusRing   :: F.FocusRing Name      -- Brick's focus rings (for tracking focus on input fields)
       , _editIp      :: E.Editor String Name  -- IP Edit field
       , _editPswd    :: E.Editor String Name  -- password edit field
       , _mode        :: Mode                  -- currently displayed screen
       , _game        :: Maybe Game            -- the game
       , _connChan    :: BChan NetworkEvent    -- connection event channel
       , _playerBoard :: Board                 -- the player's board
       , _cursorPos   :: Coord                 -- where the cursor is for placing ships
       }

makeLenses ''St


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
  ) where

import Lens.Micro.TH

import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F

import Game(Game(..), NetworkEvent(..))
import Brick.BChan (BChan)

-- Widget Names
data Name = EditIp | EditPswd deriving (Ord, Show, Eq)

-- Modes of the app
data Mode = Inputting | Joining | HostingSetup | Hosting deriving (Eq)

-- App state
data St =
    St { _focusRing :: F.FocusRing Name      -- Brick's focus rings (for tracking focus on input fields)
       , _editIp    :: E.Editor String Name  -- IP Edit field
       , _editPswd  :: E.Editor String Name  -- password edit field
       , _mode      :: Mode                  -- currently displayed screen
       , _game      :: Maybe Game            -- the game
       , _connChan  :: BChan NetworkEvent    -- connection event channel
       }

makeLenses ''St

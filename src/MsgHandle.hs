module MsgHandle
  ( readyStr
  , noShipsLeftStr
  , noShipsLeftMsg
  , readyMsg
  , shotMsg
  , shipDownMsg
  , shotResponseMsg
  , parseShot
  , parseShotResponse
  , parseShipDown
  , getCannotPlaceInfo
  , getPlacedInfo
  , getOppShotInfo
  , getShotResultInfo
  , getShipSunkInfo
  , showCoord
  , getCannotShootInfo
  , getNotYourTurnInfo
  ) where

import Ships

import Data.List (stripPrefix)
import Text.Read (readMaybe)
-- ------------------------------------------------------------------------
-- DELIMS, SEPARATORS
-- ------------------------------------------------------------------------
msgSeparator:: String
msgSeparator = "\n"

-- used with SHIPDOWN;(0,0)-(0,3);4
mainDelim :: String
mainDelim = ";"

-- used with SHIPDOWN;(0,0)-(0,3);4
coordDelim :: String
coordDelim = "-"

-- ------------------------------------------------------------------------
-- STRs and PREFIXes
-- ------------------------------------------------------------------------
readyStr :: String
readyStr = "READY"

noShipsLeftStr :: String
noShipsLeftStr = "NOSHIPSLEFT"

shotMsgPrefix :: String
shotMsgPrefix = "SHOT"

missMsgPrefix :: String
missMsgPrefix = "MISS"

hitMsgPrefix :: String
hitMsgPrefix = "HIT" 

shipDownPrefix :: String
shipDownPrefix = "SHIPDOWN"

-- ------------------------------------------------------------------------
-- MESSAGES
-- ------------------------------------------------------------------------
readyMsg :: String
readyMsg = makeMsg readyStr

noShipsLeftMsg :: String
noShipsLeftMsg = makeMsg noShipsLeftStr

shotMsg :: Coord -> String
shotMsg c = makeMsg $ shotMsgPrefix ++ coordToStr c

missMsg :: Coord -> String
missMsg c = makeMsg $ missMsgPrefix ++ coordToStr c 

hitMsg :: Coord -> String
hitMsg c = makeMsg $ hitMsgPrefix ++ coordToStr c

shotResponseMsg :: Cell -> Coord -> String
shotResponseMsg result c =
  case result of
                 Hit  -> hitMsg c
                 _    -> missMsg c 

shipDownMsg :: UserShip -> String
shipDownMsg ship =
  makeMsg $ shipDownPrefix ++ mainDelim
         ++ coordToStr (start ship) ++ coordDelim ++ coordToStr (end ship)
         ++ mainDelim ++ show (len ship)


-- ------------------------------------------------------------------------
-- PARSERS
-- ------------------------------------------------------------------------
parseShot :: String -> Maybe Coord
parseShot s = do
  rest <- stripPrefix shotMsgPrefix s
  parseCoord rest

parseShotResponse :: String -> Maybe (Cell, Coord)
parseShotResponse s
  | Just rest <- stripPrefix hitMsgPrefix s
  , Just coord <- parseCoord rest = Just (Hit, coord)
  | Just rest <- stripPrefix missMsgPrefix s
  , Just coord <- parseCoord rest = Just (Miss, coord)
  | otherwise = Nothing

parseShipDown :: String -> Maybe (Coord, Coord, ShipLen)
parseShipDown s = do
  rest1 <- stripPrefix (shipDownPrefix ++ mainDelim) s
  let (coordsPart, rest2) = break (== ';') rest1
  rest3 <- stripPrefix ";" rest2
  let (lenStr, _) = break (== '\n') rest3
  -- coordsPart: "(0,0)-(0,3)"
  let (startStr, endStrWithDash) = break (== '-') coordsPart
  endStr <- stripPrefix "-" endStrWithDash
  startCoord <- parseCoord startStr
  endCoord   <- parseCoord endStr
  len <- readMaybe lenStr
  return (startCoord, endCoord, len)

-- ------------------------------------------------------------------------
-- OTHER
-- ------------------------------------------------------------------------
getCannotPlaceInfo :: Coord -> Coord -> String
getCannotPlaceInfo c1 c2 =
  "Cannot place ship from " ++ showCoord c1 ++ " to " ++ showCoord c2

getPlacedInfo :: Coord -> Coord -> String
getPlacedInfo c1 c2 =
  "Placed ship from " ++ showCoord c1 ++ " to " ++ showCoord c2

showCoord :: Coord -> String
showCoord (r, c) = [toEnum (fromEnum 'A' + c)] ++ show (r + 1)

getOppShotInfo :: Cell -> Coord -> String
getOppShotInfo Hit  coord = "OPP hit on "  ++ showCoord coord
getOppShotInfo _ coord = "OPP missed on " ++ showCoord coord

getShotResultInfo :: Cell -> Coord -> String
getShotResultInfo Hit  coord = "You hit on "  ++ showCoord coord
getShotResultInfo _ coord = "You missed on " ++ showCoord coord

getShipSunkInfo :: String -> Coord -> Coord -> ShipLen -> String
getShipSunkInfo whose c1 c2 len = 
    whose ++ " ship sunk: " ++ showCoord c1 ++ " - " ++ showCoord c2 ++ ", size: " ++ show len

getCannotShootInfo :: Coord -> String
getCannotShootInfo  coord = "Cannot shoot at: " ++ showCoord coord

getNotYourTurnInfo :: String
getNotYourTurnInfo = "You can't shoot now - it's not your turn."
-- ------------------------------------------------------------------------
-- INSIDE HELPERS
-- ------------------------------------------------------------------------
makeMsg :: String -> String
makeMsg msg = msg ++ msgSeparator


coordToStr :: Coord -> String
coordToStr (x, y) = "(" ++ show x ++ "," ++ show y ++ ")"

parseCoord :: String -> Maybe Coord
parseCoord s = do
  s1 <- stripPrefix "(" s
  let (xStr, rest1) = break (== ',') s1
  rest2 <- stripPrefix "," rest1
  let (yStr, rest3) = break (== ')') rest2
  _ <- stripPrefix ")" rest3
  x <- readMaybe xStr
  y <- readMaybe yStr
  return (x, y)



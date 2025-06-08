module Ships
    ( Coord
    , ShipLen
    , ShipCount
    , Lifes
    , ShipMap
    , UserShip(..)
    , Cell(..)
    , Board
    , RemainingShips
    , defaultShips
    , emptyBoard
    , emptyShipMap
    , addShipToMap
    , adjacentCoords
    , canPlace
    , getCell
    , setCell
    , setShip
    , inBounds
    , placeShip
    , shipLenAvailable
    , decrementShips
    , move
    , handleShot
    , handleSunkShip
    , canShootAt
    , allShipsSunk
    ) where

import qualified Data.Map as M

import Config as Cnf


type Coord = (Int, Int)

type ShipLen = Int --- ship length

type ShipCount = Int

type Lifes = Int

data UserShip = UserShip
  { start     :: Coord
  , end       :: Coord
  , len       :: ShipLen
  } deriving (Eq, Show, Ord)

type ShipMap = M.Map UserShip Lifes

data Cell = Empty | Ship | Hit | Miss deriving (Eq)

type Board = [[Cell]]

type RemainingShips = M.Map ShipLen ShipCount

defaultShips :: RemainingShips
defaultShips = M.fromList
  [ (2, 2)
  , (3, 2)
  , (4, 1)
  , (5, 1)
  ]

emptyBoard :: Board
emptyBoard = replicate Cnf.mapSize (replicate Cnf.mapSize Empty)  -- ~ = unknown

emptyShipMap :: ShipMap
emptyShipMap = M.empty

--- częściowa aplikacja
addShipToMap :: UserShip -> ShipMap -> ShipMap
addShipToMap ship = M.insert ship (len ship)

shipCoords :: Coord -> Coord -> Maybe [Coord]
shipCoords (x1, y1) (x2, y2)
  | x1 == x2 && y1 /= y2 = Just [ (x1, y) | y <- range y1 y2 ]
  | y1 == y2 && x1 /= x2 = Just [ (x, y1) | x <- range x1 x2 ]
  | otherwise            = Nothing
  where
    range a b = if a <= b then [a..b] else [b..a]

adjacentCoords :: Coord -> [Coord]
adjacentCoords (x, y) =
  [ (x+dx, y+dy)
  | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0), inBounds (x+dx, y+dy)
  ]

canPlace :: Board -> [Coord] -> Bool
canPlace board coords =
  all (\c -> getCell board c == Empty) coords &&
  all (\c -> all (\n -> getCell board n == Empty) (adjacentCoords c)) coords

getCell :: Board -> Coord -> Cell
getCell board (x, y) = board !! x !! y

setCell :: Board -> Coord -> Cell -> Board
setCell board (x, y) val =
  take x board ++
  [take y (board !! x) ++ [val] ++ drop (y+1) (board !! x)] ++
  drop (x+1) board

setShip :: Board -> [Coord] -> Board
setShip = foldl (\b c -> setCell b c Ship)

inBounds :: Coord -> Bool
inBounds (x, y) = x >= 0 && x < Cnf.mapSize && y >= 0 && y < Cnf.mapSize

placeShip :: Coord -> Coord -> RemainingShips -> Board -> Maybe (Board, RemainingShips, UserShip)
placeShip c1 c2 availableShips board
    | fst c1 /= fst c2 && snd c1 /= snd c2 = Nothing 
    | otherwise = do 
        coords <- shipCoords c1 c2
        let shipLength = length coords

        if (shipLenAvailable shipLength availableShips) && all inBounds coords && canPlace board coords
          then
            let newBoard = setShip board coords
                newShips = decrementShips shipLength availableShips
            in Just (newBoard, newShips, UserShip (minimum coords) (maximum coords) (length coords))
          else Nothing

shipLenAvailable :: ShipLen -> RemainingShips -> Bool
shipLenAvailable len ships = do
  case M.lookup len ships of
    Just n  -> n > 0
    Nothing -> False

decrementShips :: Int -> RemainingShips -> RemainingShips
decrementShips len ships = M.update dec len ships
   where dec n = if n > 1 then Just (n-1) else Nothing

move :: Coord -> Coord -> Coord
move (dr, dc) (r, c) = (max 0 (min 9 (r+dr)), max 0 (min 9 (c+dc)))

handleSunkShip :: Board -> Coord -> Coord -> Board
handleSunkShip board c1 c2 =
  let shipCoordsList = case shipCoords c1 c2 of
                         Just cs -> cs
                         Nothing -> []
      -- Wszystkie pola wokół statku (w tym rogi)
      around = [ n | c <- shipCoordsList, n <- adjacentCoords c ]
      -- Tylko te, które są puste (nie są częścią statku i są w granicach)
      toMark = filter (\coord -> inBounds coord && getCell board coord == Empty) around
  in foldl (\b c -> setCell b c Miss) board toMark

shoot :: Coord -> Board -> Cell
shoot coord board =
  case getCell board coord of
    Ship  -> Hit
    _     -> Miss

takeHit :: Coord -> ShipMap -> (ShipMap, Maybe UserShip)
takeHit coord shipMap =
  case M.foldrWithKey match Nothing shipMap of
    Just (ship, lifes) ->
      let newLifes = lifes - 1
          newMap = M.insert ship newLifes shipMap
          sunkShip = if newLifes == 0 then Just ship else Nothing
      in (newMap, sunkShip)
    Nothing -> (shipMap, Nothing)
  where
    match ship lifes acc
      | coordInShip coord ship = Just (ship, lifes)
      | otherwise              = acc

    coordInShip (r, c) (UserShip (r1, c1) (r2, c2) _) =
      (r1 == r2 && r == r1 && c >= min c1 c2 && c <= max c1 c2) ||
      (c1 == c2 && c == c1 && r >= min r1 r2 && r <= max r1 r2)

handleShot :: Coord -> ShipMap -> Board -> (Cell, ShipMap, Board, Maybe UserShip)
handleShot coord shipMap board =
  let result = shoot coord board
      newBoard = setCell board coord result
      (newShipMap, sunkShip) = if result == Hit
                                   then takeHit coord shipMap
                                   else (shipMap, Nothing)
  in (result, newShipMap, applyHandleSunkShip newBoard sunkShip, sunkShip)

applyHandleSunkShip :: Board -> Maybe UserShip -> Board
applyHandleSunkShip board (Just ship) = handleSunkShip board (start ship) (end ship)
applyHandleSunkShip board Nothing         = board


canShootAt :: Board -> Coord -> Bool
canShootAt board coord =
  inBounds coord && (cell == Empty)
  where cell = getCell board coord

allShipsSunk :: ShipMap -> Bool
allShipsSunk = all (== 0) . M.elems
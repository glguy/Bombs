module Simulation where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Either (partitionEithers)

-- seconds
bombDuration :: Float
bombDuration = 3

type Coord = (Int,Int)

data Bomb = Bomb
  { bombOwner :: Int
  , bombCoord :: Coord
  , bombTimer :: Float
  }

data World = World
  { players :: Map Int Coord
  , bombs   :: [Bomb]
  }

data Direction
  = U | D | L | R
  deriving (Read, Show, Eq)

minX,maxX,minY,maxY :: Int
minX = -4
minY = -4
maxX = 4
maxY = 4

isValidCoord :: Coord -> Bool
isValidCoord (x,y) = (even x || even y)
                  && x <= maxX 
                  && y <= maxY 
                  && minX <= x
                  && minY <= y

startingWorld = World Map.empty []

placeBomb :: Int -> Coord -> World -> World
placeBomb i c w = w { bombs = b : bombs w }
  where
  b = Bomb
        { bombOwner = i
        , bombCoord = c
        , bombTimer = bombDuration
        }

removeBomb :: Coord -> World -> World
removeBomb c w = w { bombs = [b | b <- bombs w, bombCoord b /= c] }

addEntity :: Int -> Coord -> World -> World
addEntity i c w = w { players = Map.insert i c $ players w }

removeEntity :: Int -> World -> World
removeEntity i w = w { players = Map.delete i $ players w }

moveEntity :: Int -> Direction -> World -> Maybe (World, Coord)
moveEntity i d w =
  case Map.lookup i m of
    Just c
      | isValidCoord c' && not (c' `elem` Map.elems m) ->
         Just (w { players = Map.insert i c' m}, c')
      where
      c' = moveCoord d c
    _ -> Nothing
  where
  m = players w

moveCoord :: Direction -> Coord -> Coord
moveCoord U (x,y) = (x,y+1)
moveCoord D (x,y) = (x,y-1)
moveCoord L (x,y) = (x-1,y)
moveCoord R (x,y) = (x+1,y)

timeStepWorld :: Float -> World -> (World, [Bomb])
timeStepWorld elapsed w = (w { bombs = bombs' }, exploded)
  where
  (bombs', exploded) = partitionEithers $ map updateBomb $ bombs w

  updateBomb b
    | timer' > 0 = Left b { bombTimer = timer' }
    | otherwise = Right b
    where
    timer' = bombTimer b - elapsed

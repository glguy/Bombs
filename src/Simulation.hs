{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Simulation where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Either (partitionEithers)

-- seconds
bombDuration :: Float
bombDuration = 3

type Coord = (Int,Int)

data Bomb = Bomb
  { _bombOwner :: Int
  , _bombCoord :: Coord
  , _bombTimer :: Float
  }
  deriving (Show)

data World = World
  { _players :: Map Int Coord
  , _bombs   :: [Bomb]
  }

makeLenses ''Bomb
makeLenses ''World

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

startingWorld = World
  { _players = Map.empty
  , _bombs   = []
  }

placeBomb :: MonadState World m => Int -> Coord -> m ()
placeBomb i c = bombs %= (b :)
  where
  b = Bomb
        { _bombOwner = i
        , _bombCoord = c
        , _bombTimer = bombDuration
        }

removeBomb :: MonadState World m => Coord -> m ()
removeBomb c = bombs %= \xs -> [b | b <- xs, b^.bombCoord /= c]

addEntity :: MonadState World m => Int -> Coord -> m ()
addEntity i c = players %= Map.insert i c

removeEntity :: MonadState World m => Int -> m ()
removeEntity i = players %= Map.delete i

moveEntity :: MonadState World m => Int -> Direction -> m (Maybe Coord)
moveEntity i d =
  do m <- use players
     case Map.lookup i m of
       Just c
         | isValidCoord c' && not (c' `elem` Map.elems m) ->
            do players %= Map.insert i c'
               return (Just c')
         where
         c' = moveCoord d c
       _ -> return Nothing

moveCoord :: Direction -> Coord -> Coord
moveCoord U (x,y) = (x,y+1)
moveCoord D (x,y) = (x,y-1)
moveCoord L (x,y) = (x-1,y)
moveCoord R (x,y) = (x+1,y)

timeStepWorld :: Float -> State World [Bomb]
timeStepWorld elapsed =
  do bs <- use bombs
     let (bs', exploded) = partitionEithers $ map updateBomb bs
     bombs .= bs'
     return exploded
  where

  updateBomb b
    | timer' > 0 = Left (bombTimer .~ timer' $ b)
    | otherwise = Right b
    where
    timer' = b^.bombTimer - elapsed

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

-- seconds
explosionDuration :: Float
explosionDuration = 1

startingPower :: Int
startingPower = 3

startingBombs :: Int
startingBombs = 1

type Coord = (Int,Int)

data Bomb = Bomb
  { _bombOwner :: Int
  , _bombPower :: Int
  , _bombCoord :: Coord
  , _bombTimer :: Float
  , _bombExploded :: Bool
  }
  deriving (Show)

data Player = Player
  { _playerCoord :: Coord
  , _playerBombs :: Int
  , _playerPower :: Int
  }

data World = World
  { _players :: Map Int Player
  , _bombs   :: [Bomb]
  }

data Direction = U | D | L | R
  deriving (Read, Show, Eq)

makeLenses ''Bomb
makeLenses ''Player
makeLenses ''World

startingWorld :: World
startingWorld = World
  { _players = Map.empty
  , _bombs   = []
  }

newPlayer :: Coord -> Player
newPlayer c = Player
  { _playerCoord = c
  , _playerBombs = startingBombs
  , _playerPower = startingPower
  }

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

placeBomb :: MonadState World m => Int -> Int -> Coord -> m ()
placeBomb i p c = bombs %= (b :)
  where
  b = Bomb
        { _bombOwner = i
        , _bombCoord = c
        , _bombTimer = bombDuration
        , _bombPower = p
        , _bombExploded = False
        }

explodeBomb :: MonadState World m => Coord -> m ()
explodeBomb c = bombs %= fmap f
  where f b | b^.bombCoord == c = bombExploded .~ True $ b
            | otherwise         = b

removeBomb :: MonadState World m => Coord -> m ()
removeBomb c = bombs %= \bs -> [b | b <- bs, b^.bombCoord /= c]

addPlayer :: MonadState World m => Int -> Coord -> m ()
addPlayer i c = player i .= newPlayer c

incrementBombs :: MonadState World m => Int -> m ()
incrementBombs i = player i . playerBombs += 1

decrementBomb :: MonadState World m => Int -> m Bool
decrementBomb i =
  do p <- use $ player i
     let hasBombs = p ^. playerBombs > 0
     when hasBombs $ player i . playerBombs -= 1
     return hasBombs

removePlayer :: MonadState World m => Int -> m ()
removePlayer i = players %= Map.delete i

movePlayer :: MonadState World m => Int -> Direction -> m (Maybe Coord)
movePlayer i d =
  do p  <- use $ player i
     let c' = p ^. playerCoord ^% moveCoord d

     bs <- uses bombs   $ fmap (view bombCoord)
     m  <- uses players $ fmap (view playerCoord) . Map.elems

     if isValidCoord c' && not (c' `elem` (m ++ bs))
       then do player i . playerCoord .= c'
               return (Just c')
       else return Nothing

moveCoord :: Direction -> Coord -> Coord
moveCoord U (x,y) = (x,y+1)
moveCoord D (x,y) = (x,y-1)
moveCoord L (x,y) = (x-1,y)
moveCoord R (x,y) = (x+1,y)

timeStepWorld :: MonadState World m => Float -> m ([Bomb],[Bomb])
timeStepWorld elapsed =
  do bombs . mapped . bombTimer -= elapsed
     bs <- use bombs
     let normal    = [ b | b <- bs, b^.bombTimer > 0]
         detonated = [ bombExploded.~ True 
                     $ bombTimer   .~ explosionDuration
                     $ b | b <- bs, b^.bombTimer <= 0, not(b^.bombExploded)]
         finished =  [ b | b <- bs, b^.bombTimer <= 0, b^.bombExploded ]
     forM_ detonated $ \b -> incrementBombs $ b^.bombOwner
     bombs .= normal ++ detonated
     return (detonated, finished)

player :: Int -> Simple Lens World Player
player i = lens lkup (\w p -> players %~ Map.insert i p $ w)
  where
  lkup w = case Map.lookup i $ w^.players of
             Nothing -> error "players"
             Just p  -> p

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
startingBombs = 2

type Coord = (Int,Int)

data Bomb = Bomb
  { _bombOwner :: Int
  , _bombPower :: Int
  , _bombTimer :: Float
  , _bombExploded :: Bool
  }
  deriving (Show, Read)

data Player = Player
  { _playerCoord :: Coord
  , _playerBombs :: Int
  , _playerPower :: Int
  }
  deriving (Show, Read)

data World = World
  { _players :: Map Int Player
  , _tiles   :: Map Coord Tile
  }
  deriving (Show, Read)

data Tile
  = BombTile Bomb
  | RockTile
  deriving (Show, Read)

data Direction = U | D | L | R
  deriving (Read, Show, Eq)

makeLenses ''Bomb
makeLenses ''Player
makeLenses ''World

startingWorld :: World
startingWorld = World
  { _players = Map.empty
  , _tiles   = Map.empty
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

placeBomb :: MonadState World m => Int -> Coord -> m ()
placeBomb i c =
  do b <- newBomb i
     tile c .= Just (BombTile b)

newBomb :: MonadState World m => Int -> m Bomb
newBomb i =
  do power <- use $ player i . playerPower
     return Bomb
       { _bombOwner     = i
       , _bombTimer     = bombDuration
       , _bombPower     = power
       , _bombExploded  = False
       }

explodeBomb :: MonadState World m => Coord -> m ()
explodeBomb c = tile c . mapped . bomb . bombExploded .= True

removeBomb :: MonadState World m => Coord -> m ()
removeBomb c = tiles %= Map.delete c

addPlayer :: MonadState World m => Int -> Coord -> m ()
addPlayer i c = player i .= newPlayer c

incrementBombs :: MonadState World m => Int -> m ()
incrementBombs i = player i . playerBombs += 1

decrementBomb :: MonadState World m => Int -> m Bool
decrementBomb i =
  do n <- use $ player i . playerBombs
     let hasBombs = n > 0
     when hasBombs $ player i . playerBombs .= n-1
     return hasBombs

removePlayer :: MonadState World m => Int -> m ()
removePlayer i = players %= Map.delete i

movePlayer :: MonadState World m => Int -> Direction -> m (Maybe Coord)
movePlayer i d =
  do c' <- uses (player i . playerCoord) (moveCoord d)
     bs <- uses tiles   Map.keys
     ps <- uses players Map.elems
     let m = map (view playerCoord) ps

     if isValidCoord c' && not (c' `elem` (m ++ bs))
       then do player i . playerCoord .= c'
               return (Just c')
       else return Nothing

moveCoord :: Direction -> Coord -> Coord
moveCoord U (x,y) = (x,y+1)
moveCoord D (x,y) = (x,y-1)
moveCoord L (x,y) = (x-1,y)
moveCoord R (x,y) = (x+1,y)

timeStepWorld :: MonadState World m => Float -> m ([Coord],[Coord])
timeStepWorld elapsed =
  do tiles . mapped . bomb . bombTimer -= elapsed
     bs <- use bombs

     let normal    = [ (i,b) | (i,b) <- bs, b^.bombTimer > 0]
         detonated = [ (i, bombExploded.~ True 
                         $ bombTimer   .~ explosionDuration
                         $ b)
                     | (i,b) <- bs, b^.bombTimer <= 0, not(b^.bombExploded)]
         finished =  [ i | (i,b) <- bs, b^.bombTimer <= 0, b^.bombExploded ]

     forM_ normal $ \(i,b) ->
       tile i .= Just (BombTile b)

     forM_ detonated $ \(i,b) ->
       do tile i .= Just (BombTile b)
          incrementBombs $ bombOwner ^$ b

     forM_ finished $ \i ->
       tile i .= Nothing

     return (map fst detonated, finished)

player :: Int -> Simple Lens World Player
player i = lens lkup (\w p -> players %~ Map.insert i p $ w)
  where
  lkup w = case Map.lookup i $ players ^$ w of
             Nothing -> error "players"
             Just p  -> p

tile :: Coord -> Simple Lens World (Maybe Tile)
tile i = tiles . mapping i

bomb :: Simple Setter Tile Bomb
bomb = sets aux
  where
  aux f (BombTile b) = BombTile (f b)
  aux _ RockTile     = RockTile

bombs :: Getter World [(Coord,Bomb)]
bombs = to $ \w -> [(c,b) | (c,BombTile b) <- Map.toList $ tiles ^$ w]

mapping :: Ord k => k -> Simple Lens (Map k v) (Maybe v)
mapping k = lens (Map.lookup k)
                 (\m mb -> case mb of
                             Just v  -> Map.insert k v m
                             Nothing -> Map.delete k m)


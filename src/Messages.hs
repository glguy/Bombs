module Messages where

import Data.Binary

import Simulation

data ServerMessage
  = SetWorld [(Int,Coord)]
  | MoveEntity Int Coord
  | AddBomb      Coord
  | DetonateBomb Coord
  | DeleteEntity Int
  deriving (Read, Show)

data ClientMessage
  = Move Direction
  | DropBomb
  deriving (Read, Show)

-- XXX: These are some of the worst instances.

instance Binary ServerMessage where
  get = fmap read get
  put = put . show

instance Binary ClientMessage where
  get = fmap read get
  put = put . show

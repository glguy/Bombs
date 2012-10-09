module Main where

import Control.Monad (forM_)
import Data.List
import Data.Map (Map)
import Network(PortID(PortNumber))
import qualified Data.Map as Map

import Framework.Server (NetworkServer(..), serverMain, Handles, ConnectionId, announce, announceOne)
import Messages
import Simulation

main = serverMain myNetworkServer initialState

myNetworkServer = NetworkServer
  { serverPort          = PortNumber 15000
  , eventsPerSecond     = 10
  , onTick              = tick
  , onConnect           = connect
  , onDisconnect        = disconnect
  , onCommand           = command
  }

data ServerState = ServerState
  { serverWorld         :: World
  , playerMapping       :: Map ConnectionId Int
  }

initialState = ServerState
  { serverWorld = startingWorld
  , playerMapping = Map.empty
  }

tick :: Handles ConnectionId -> Float -> ServerState -> IO ServerState
tick hs elapsed s =
  do forM_ exploded $ \b ->
       announce hs $ DetonateBomb $ bombCoord b
     return s { serverWorld = w' }
  where
  (w',exploded) = timeStepWorld elapsed $ serverWorld s

connect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
connect hs i s =
  do announceOne hs i $ SetWorld $ Map.toList $ players w
     announce    hs   $ MoveEntity entityId (0,0)
     return $ s { playerMapping = Map.insert i entityId (playerMapping s)
                , serverWorld   = w
                }
  where
  entityId = head $ [0..] \\ Map.elems (playerMapping s)
  w = addEntity entityId (0,0) (serverWorld s)

disconnect ::
  Handles ConnectionId ->
  ConnectionId ->
  ServerState ->
  IO ServerState
disconnect hs i s = 
  do announce hs $ DeleteEntity entityId
     return s { playerMapping = Map.delete i (playerMapping s)
              , serverWorld   = removeEntity entityId (serverWorld s)
              }
  where
  Just entityId = Map.lookup i (playerMapping s)

command ::
  Handles ConnectionId ->
  ConnectionId ->
  ClientMessage ->
  ServerState ->
  IO ServerState
command hs i c s =
  case c of
    Move dir ->
      case moveEntity entityId dir (serverWorld s) of
        Nothing         -> return s
        Just (w',coord) ->
          do announce hs $ MoveEntity entityId coord
             return s { serverWorld = w' }

    DropBomb ->
      do let w' = placeBomb entityId playerCoord w
         announce hs $ AddBomb playerCoord
         return s { serverWorld = w' }
  where
  w = serverWorld s
  Just entityId = Map.lookup i (playerMapping s)
  Just playerCoord = Map.lookup entityId (players w)
